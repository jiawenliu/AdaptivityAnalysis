(* ---------------------------------------------------------------------- *)
(* typechecking engine for Adapt Program                               *)
(* ---------------------------------------------------------------------- *)

open Tycheck
module UnaryTy =
struct
      type ty = Syntax.un_ty
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
let rec check_equiv (ty1 : ty) (ty2 : ty) : constr checker =
  let fail = fail NotSubtype (ty1, ty2) in
  	if ty1 = ty2 then return_ch empty_constr
  	else 
  match ty1, ty2 with
  	(* CASES WHEN THE FIRST TYPE IS A BOX TYPE*)
  | Ty_Box bty1, ty2 -> 
  	begin
  	match bty1, ty2 with
  		| ty, ty                 -> return_ch empty_constr
  		| ty, Ty_Box (Ty_Box ty) -> return_ch empty_constr
  		| bty1, Ty_Box bty2      -> check_equiv bty1 bty2
  		| Ty_Arrow(ity, d, dmap, ad, oty), Ty_Arrow(Ty_Box ity, 0, dmap, IConst 0, Ty_Box oty)
  								 -> return_ch empty_constr
  		| _                      -> fail

    end

  (* CASES WHEN THE SECOND TYPE IS A BOX TYPE*)
  | bty1, Ty_Box bty2 -> 
    begin
    match bty1, bty2 with
    	| Ty_Prim pty1, Ty_Prim pty2 -> 
    		if pty1 = pty2 then return_ch empty_constr
    					         else fail
      | Ty_List Ty_Box lty1, Ty_List lty2 ->
        if lty1 = lty2 then return_ch empty_constr
                       else fail

    	| _ -> fail
    end 

  (* CASES WHEN NONE OF THE TYPES IS A BOX TYPE*)
  | Ty_IntIndex i, Ty_Prim Ty_PrimInt -> return_ch empty_constr

  | Ty_Prod(sty1, sty2), Ty_Prod(sty1', sty2') 
  						   -> check_equiv sty1 sty1' >> check_equiv sty1' sty2'

  | Ty_Arrow(ity, q, dmap, ad, oty), Ty_Arrow(ity', q', dmap', ad', oty') 
  						   -> if q < q' then
  						   	check_size_leq ad ad' (check_equiv ity' ity >> check_equiv oty oty')
  						   else
  						   	fail

  | Ty_List(ty1), Ty_List(ty2)
                 -> check_equiv ty1 ty2

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
    | True | False
                   -> return_inf(Ty_Bool) <<= infer_bool 
    |  _           -> fail (expInfo e) (Internal ("no inference rule, try annotating the expression please."))


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
              let dps' = sum_cons_dmap (IDConst 1) (max_dmap dps (sum_dmap_adap z dps'')) in
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
  | ILam (e), Ty_Forall(x, s, mu, k, ty) 
        ->
            if (s = Loc) then
             begin
              check_body ((x |::::| s) dp
                            (with_mode mu (checkType e ty))) k
                end
            else    check_body ((x |::| s) dp
                        (with_mode mu (checkType e ty))) k
  (* Existential introduction and elimination *)
  | Pack (e), _ 
        -> check_pack dp e ty

  | Unpack (e1, vi_x, e2), _ 
        -> check_unpack dp e1 vi_x e2 ty

  (* Let bound *)
  | Let (vi_x, e1, e2), _ 
        ->
          inferType e1 <->=
          (fun ty_x -> (vi_x  |:| ty_x) (checkType e2 ty))

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
              let cs2 = dmap_cs dps' dps in

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


and check_pack i e ty =
  match ty with
  | Ty_Exists(x, s, ty) ->
    let v = fresh_evar s in
    let witness = IVar v in
    (v |:::| s) i (checkType e (un_ty_subst x witness ty))
  | _ -> fail i (WrongShape (ty, "existential"))

and check_unpack i e1 vi_x e2 ty =
  inferType e1 <->=
  (fun ty_x ->
     match ty_x with
     | Ty_Exists(x, s, ty) ->
       (x |::| s) i ((vi_x |:| ty) (checkType e2 ty))
     | _ -> fail i (WrongShape (ty, "existential")))



and infer_and_check (i: info) (e: expr) (ty : ty) : constr checker =
  fun(ctx, k) ->
    match inferType e ctx with
    | Right (inf_ty, c, psi_ctx, k') ->
      debug dp "infer_and_check :@\n@[infer_type is %a, expr is %a, checked type is %a, idx_ctx is :%a , k' is %a, k is %a @]@." 
      Print.pp_type inf_ty Print.pp_expr e Print.pp_type ty Print.pp_ivar_ctx ctx.ivar_ctx Print.pp_adapt k' Print.pp_adapt k; 
      (
        match (check_equiv inf_ty ty (extend_e_ctx psi_ctx ctx, None)) with
          | Right c' -> 
          	  let 
                cs = option_combine k' k (cost_cs ctx) |> Core.Option.value ~default:CTrue 
              in
                debug dp "infer_and_check2 :@\n@[cs is %a, c is %a, c' is %a @]@." 
                Print.pp_cs cs Print.pp_cs c Print.pp_cs c' ; 
                Right (quantify_all_exist psi_ctx (merge_cs (merge_cs c c') cs))
          | Left err -> Left err
      )
    | Left err' -> Left err'


let check_type ctx program ty  =
  match checkType program ty ctx with
  | Right c ->  c
  | Left err -> typing_error_pp  err.i pp_tyerr err.v


 
