(* ---------------------------------------------------------------------- *)
(* typechecking engine for Adapt Program                               *)
(* ---------------------------------------------------------------------- *)

open Tycheck
module UnaryTy =
struct
      type ty = Syntax.ty
    end
    
module AbstractUnary = TyCheck(UnaryTy)
     
open AbstractUnary
open Fresh_var
open Syntax
open IndexSyntax
open Constr
open Ctx
open Ty_error
open Support.FileInfo
open Map
open DMap

module Opt = Support.Options
module Err = Support.Error

let dp = Support.FileInfo.dummyinfo
let debug   fi = Support.Error.message 4 General fi

let typing_error fi = Err.error_msg    Opt.TypeChecker fi
let typing_error_pp = Err.error_msg_pp Opt.TypeChecker


(** Check whether ty1 is a subtype of ty2, generating the necessary
    constraints along the way. **)
let rec check_equiv (ty1 : ty) (ty2 : ty) : constr equiv_checker =
  let fail = fail NotSubtype (ty1, ty2) in
  	(* BASE Case For Equivalent *)
    if ty1 = ty2 then return_leaf_eq_ch
  	else 
      match ty1, ty2 with
      	(* CASES WHEN THE FIRST TYPE IS A BOX TYPE*)
        | Ty_Prod(sty1, sty2), Ty_Prod(sty1', sty2') 
                       -> check_equiv sty1 sty1' << check_equiv sty1' sty2'


        (* CASES WHEN NONE OF THE TYPES IS A BOX TYPE*)
        | Ty_IntIndex i1, Ty_IntIndex i2 -> return_eq_ch (CEq(i, i1))


        | Ty_Arrow(ity, q, dps, z, oty), Ty_Arrow(ity', q', dps', z', oty') 
        						   -> 
                        let cs1 = depth_eq q q' in
                          let cs2 = adap_eq z z' in
                            let cs3 = dmap_eq dps dps' in
        						   	      (check_equiv ity' ity << check_equiv oty oty') 
                              <=<
                              merge_cs cs1 @@ merge_cs cs2 cs3 

        | Ty_Box ty1, Ty_Box ty2 
                       -> check_equiv ty1 ty2

        | Ty_List ty1, Ty_List ty2
                       -> check_equiv ty1 ty2

        | Ty_Forall(ix1, s1, dps1, z1, ty1), Ty_Forall(ix2, s2, dps2, z2, ty2)
                        -> let cs1 = adap_eq z1 z2 in
                            let cs2 = dmap_eq dps1 dps2 in
                              (check_equiv ty1 ty2) <=< (merge_cs cs1 cs2)

        | _ , _ -> fail

                 
(** [inferType e] infers that expression [e] has type [un_ty] along
    with cost [k] in context [ctx]. If it does, it returns [un_ty
    inferer: (un_ty, k, psi)] where all the free existential variables
    occuring in un_ty and k are declared in psi, otherwise it raises
    an exception. *)
let rec inferType (e: expr) : ty inferer  =
  let _ = debug dp "infer_TP:@\n@[e1 %a @]@.@\n"  Print.pp_expr e in
    match e with
    | Var(vi)     -> get_var_ty vi <<= infer_var vi
    | Prim(ep)    -> return_inf(Syntax.type_of_prim ep) <<= infer_const
    | Fst(e)      -> inferType e <<= infer_proj fst
    | Snd(e)      -> inferType e <<= infer_proj snd
    | App(e1,e2)  -> debug dp "infer_app:@\n@[e1 %a @]@.@\n"  Print.pp_expr e1 ;
         infer_app (inferType e1) e2
    | IApp(e)     -> infer_iapp (inferType e) dp
    | Mech e      -> infer_mech (inferType e)
    (*| Anno(e, ty, dps, z) -> infer_check_anno e ty dps z*)
    | True | False
                   -> return_inf(Ty_Bool) <<= infer_bool 
    |  _ -> fail dp (Internal ("no inference rule, try annotating the expression please."))


(*and infer_check_anno e ty dps z =*)
  

and infer_var vi =
  fun ty ->
    fun ctx -> 
      let depthmap = bot_dmap ctx in
        let depthmap = set_dmap depthmap vi.v_name (DConst 0) in
          (* generate Zero Depth *)
          let d0 = DConst 0 in
            (* get depth of variable x *)
            let dx = get_depth depthmap vi.v_name in
              match dx with
                | Some dx -> Right (ty, depth_cs d0 dx, depthmap, (IConst 0) )
                | None    -> Left err


and infer_const =
  fun ctx -> 
          let depthmap = bot_dmap ctx in
            Right (ty, empty_constr, depthmap, (IConst 0) )


and infer_bool =
  fun ctx -> 
          let depthmap = bot_dmap ctx in
            Right (ty, empty_constr, depthmap, (IConst 0) )


and infer_mech m =
  fun ctx ->
    match m ctx with 
      | Right(ty, c, dps, z) ->
      begin
        match ty with 
          | Ty_Arrow(ty1, IConst 0, dm, IConst 0, ty2) ->

          (* Convert the depth map list from the arrow type into dmap  *)
          let dps'' = to_dmap dm in
           
           (*Increase the Adaptivity by 1 *)
           let z' = add_adapts z (IConst 1) in

              (*Increase the Depth map by 1 *)
              let dps' = sum_depth_dmap (IDConst 1) (max_dmap dps (sum_dmap_adap z dps'')) in
                Right ( (Ty_Prim Ty_PrimReal), c, dps', z')

          | _ -> fail dp (WrongShape (ty, " Mechanism operation Error ")) ctx
      end
      | Left err -> Left err




and infer_iapp m =
  fun ctx ->
    match m ctx with
    | Right (ty, c, dps, z) ->
    debug dp "inf_iapp2 :@\n@[c is :%a, ty  %a @]@." Print.pp_cs c Print.pp_type ty ; 
      begin
        match ty with
        | Ty_Forall(x, s, dps1, z1, ty) ->
          (* Generate New forall variable *)
          let v = fresh_evar s in
          let witn = IVar v in
          (* New Depth Map *)
          let dps' = max_dmap dps (sum_adap_dmap z (to_dmap dps1)) in
            (* New Adaptivity *)
            let z' = add_adapts (adapt_subst z1 x witn) z in 
              Right (ty_subst x witn ty, c, dps', z')
        | _ -> fail dp (WrongShape (ty, "index quantified (forall) ")) ctx
      end
    | Left err -> Left err


and infer_app m e2 =
  m =<-> (
    fun ty _ ->
      match ty with
      | Ty_Arrow(ty1, q, dps, z, ty2) ->
         debug dp "inf_app :@\n@[ty is :%a, k'' is %a, e2 is %a @]@." Print.pp_type ty Print.pp_iterm k'' Print.pp_expr e2; 
        ((checkType e2 ty1), ty2, q, to_dmap dps, z)
      | _ -> (fail dp (WrongShape (ty, "function")), ty2, IConst 0, to_dmap dps, z)
      )

  

and infer_proj f =
  fun ty ->
    match ty with
    | Ty_Prod (ty1, ty2) -> return_inf(f (ty1, ty2))
    | _ -> fail dp (WrongShape (ty, "product"))

(** [checkType e] verifies that expression [e] has type [ty]
    in the context [ctx] with the adapts [z]. If
    it does, it returns constrain checker, otherwise it raises an exception. *)
and checkType (e: expr) (ty : ty) : constr checker =
  debug dp "@[UCK  %a, ty is %a @]@." Print.pp_expr e Print.pp_type ty; 

  match e, ty with

  (* Primitive expressions *)
  |  Prim (ep), tp 
        ->
           debug dp "primTInt :@\n@[%a, %a@]@." Print.pp_expr e Print.pp_type tp  ; 
            begin
            match ep with
            | PrimInt x -> let ty_ep = Syntax.type_of_prim ep in             
                  begin
                    match ty_ep, tp with
                    | Ty_IntIndex x, Ty_IntIndex y ->  Tycheck.check_size_eq x y  Tycheck.return_leaf_ch
                    | _, Ty_Prim Ty_PrimInt -> Tycheck.return_leaf_ch
                    | _, _ -> fail dp @@ WrongShape (tp, "int[i] or int")
                  end
               
            | _ -> if tp = Syntax.type_of_prim ep
                   then Tycheck.return_leaf_ch else fail dp @@ WrongShape (tp, "primitive2")
             end
  | Fix( f, x, ty_x, e), _ 
        ->  check_fix f x ty_x e ty

  (* List type expressions *)
  | Nil, _ 
        ->  check_nil dp ty
  | Cons( e1, e2), _ 
        ->           
          begin
            match ty with
            | Ty_List (ty') -> (checkType e1 ty') >> (checkType e2 (Ty_List ty'))
            | _ -> fail dp (WrongShape (ty, "List in Cons"))
          end


  (* If statement *)
  | If( e, el, er), _ 
        ->  debug dp "checkif, el is %a, ty is %a " Print.pp_expr el Print.pp_type ty ; 
            (checkType e Ty_Bool) >&&> ((checkType el ty) >> (checkType er ty))

  (* Pairs *)
  | Pair(e1, e2), _ 
        ->
          begin
            match ty with
            | Ty_Prod (ty1,ty2) -> (checkType e1 ty1) >> (checkType e2 ty2)
            | _ -> fail dp (WrongShape (ty, "product"))
          end

  (* Index abstraction *)
  | ILam (e), Ty_Forall(ix, s, dps, z, ty)
        ->           
          (ix |::| s) (checkType e ty)

  (* Existential introduction and elimination *)
  | Pack (e), _ 
        -> check_pack e ty

  | Unpack (e1, vi_x, e2), _ 
        -> check_unpack e1 vi_x e2 ty

  (* Let bound *)
  | Let (vi_x, q, e1, e2), _ 
        ->
          inferType e1 <->=
          (fun ty_x -> ((vi_x  |:| ty_x) (checkType e2 ty), q, vi_x))

  | Bernoulli (v), _ 
        -> (checkType v (Ty_Prim Ty_PrimReal))
  | Uniform (v1, v2), _ 
        -> (checkType v1 (Ty_Prim Ty_PrimReal)) >> (checkType v1 (Ty_Prim Ty_PrimReal))

  |  _ , _ -> infer_and_check dp e ty




and check_fix (vi_x : var_info) (e : expr) (ty : ty) =
  match ty with
  | Ty_Arrow(ty1, q, dps, z, ty2) ->
(*  debug dp "ck_fix:@\n@[e %a, ty1: %a, ty2: %a @]@.@\n"  
      Print.pp_expr e Print.pp_type ty1 Print.pp_type ty2;
*)
    let m = ((vi_f |:| ty)
                  ((vi_x |:| ty1)
                    (checkType e ty2))) in
    fun ctx ->
      match m ctx with
        | Right(c, dps', z') -> 
          let depthx = (get_depth vi_x dps') in
            (* Constrain for depth of x *)
            let cs1 = (depth_cs depthx q) in

              (* Constrains for depth of others *)
              let cs2 = dmap_cs dps' (to_dmap dps) in

                (* Depth Map with All bottom*)
                let dps'' = bot_dmap ctx in
                  Right(CAnd(c, CAnd(cs1, cs2)), dps'', IConst 0) 
 
  | _ ->  fail dp (WrongShape (ty, "fuction"))






and check_nil i ty =
  match ty with
  | Ty_List(ty) 
      -> return_leaf_ch >>= (fun cs -> 
        fun ctx -> Right(empty_constr, bot_dmap ctx, IConst 0))
  | _ -> fail i (WrongShape (ty, "list"))


and check_cons e1 e2 i ty =
  match ty with
  | Ty_List(ty) -> checkType e1 ty
  | _ -> fail i (WrongShape (ty, "list"))


and check_pack e ty =
  Tycheck.return_leaf_ch

and check_unpack e1 vi_x e2 ty =
    Tycheck.return_leaf_ch


and infer_and_check (i: info) (e: expr) (ty : ty) : constr checker =
  fun ctx ->
    match inferType e ctx with
    | Right (inf_ty, c, dps, z) ->
      debug dp "infer_and_check :@\n@[infer_type is %a, expr is %a, checked type is %a, depth map is :%a , adaptivity is %a @]@." 
      Print.pp_type inf_ty Print.pp_expr e Print.pp_type ty Print.pp_dmap dps Print.pp_adapt z; 
      (
        match (check_equiv inf_ty ty ctx) with
          | Right c' -> 
              Right ((merge_cs c c'), dps, z)
          | Left err -> Left err
      )
    | Left err' -> Left err'


let check_type ctx program ty  =
  match checkType program ty ctx with
  | Right c ->  c
  | Left err -> typing_error_pp  err.i pp_tyerr err.v


 
