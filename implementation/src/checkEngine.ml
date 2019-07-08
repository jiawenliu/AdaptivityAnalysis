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


module Opt = Support.Options
module Err = Support.Error

let dp = Support.FileInfo.dummyinfo
let debug   fi = Support.Error.message 4 General fi

let typing_error fi = Err.error_msg    Opt.TypeChecker fi
let typing_error_pp = Err.error_msg_pp Opt.TypeChecker


(** Check whether ty1 is a subtype of ty2, generating the necessary
    constraints along the way. **)
let rec check_subtype (ty1 : ty) (ty2 : ty) : constr checker =
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
  		| bty1, Ty_Box bty2      -> check_subtype bty1 bty2
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
    	| 
    end 

  (* CASES WHEN NONE OF THE TYPES IS A BOX TYPE*)
  | Ty_IntIndex i, Ty_Prim Ty_PrimInt -> return_ch empty_constr

  | Ty_Prod(sty1, sty2), Ty_Prod(sty1', sty2') 
  						   -> check_subtype sty1 sty1' >> check_subtype sty1' sty2'

  | Ty_Arrow(ity, q, dmap, ad, oty), Ty_Arrow(ity', q', dmap', ad', oty') 
  						   -> if q < q' then
  						   	check_size_leq ad ad' (check_subtype ity' ity >> check_subtype oty oty')
  						   else
  						   	fail
  | _ , _ -> fail


                 
(** [inferType e] infers that expression [e] has type [un_ty] along
    with cost [k] in context [ctx]. If it does, it returns [un_ty
    inferer: (un_ty, k, psi)] where all the free existential variables
    occuring in un_ty and k are declared in psi, otherwise it raises
    an exception. *)
let rec inferType (e: expr) : ty inferer  =
  let _ = debug dp "infer_TP:@\n@[e1 %a @]@.@\n"  Print.pp_expr e in
    match e with
    | Var ( vi) -> (get_var_ty vi <<= fun ty ->  (return_inf ty))
    | Prim (ep) -> return_inf(un_type_of_prim ep )
    | Fst(e) -> inferType e <<= infer_proj dp fst
    | Snd(e) -> inferType e <<= infer_proj dp snd
    | App (e1, e2) -> debug dp "infer_app:@\n@[e1 %a @]@.@\n"  Print.pp_expr e1 ;
         infer_app (inferType e1) dp e2
    | IApp (e) -> infer_iapp (inferType e) dp
    |  _ -> fail (expInfo e) (Internal ("no inference rule, try annotating the expression please."))



and infer_iapp m i =
  fun ctx ->
    match m ctx with
    | Right (ty, c, psi, k) ->
    debug dp "inf_iapp2 :@\n@[c is :%a, ty  %a @]@." Print.pp_cs c Print.pp_type ty ; 
      begin
        match ty with
        | Ty_Forall(x, s, ty) ->
          let v = fresh_evar s in
          let witn = IVar v in
          let k' = IndexSyntax.sum_adapts k (iterm_subst x witn k_e) in 
            Right (ty_subst x witn ty, c, (v,s):: psi, k')
        | _ -> fail i (WrongShape (ty, "index quantified (forall) ")) ctx
      end
    | Left err -> Left err


and infer_app m i e2 =
  m =<-> (fun ty _ ->
      match ty with
      | Ty_Arrow(ty1, mu', dmap, k'', ty2) ->
         debug dp "inf_app :@\n@[ty is :%a, k'' is %a, e2 is %a @]@." Print.pp_type ty Print.pp_iterm k'' Print.pp_expr e2; 
        [((checkType e2 ty1), ty2, k'')]
      | _ -> [(fail i (WrongShape (ty, "function")), Ty_Prim (UPrimInt), IZero)])


and infer_proj i f =
  fun ty ->
    match ty with
    | Ty_Prod (ty1, ty2) ->  return_inf(f (ty1, ty2))
    | _ -> fail i (WrongShape (ty, "product"))

(** [checkType e] verifies that expression [e] has unary type [ty]
    in the context [ctx] with the cost [k]. If
    it does, it returns unit, otherwise it raises an exception. *)
and checkType (e: expr) (ty : ty) : constr checker =
    debug dp "@[UCK  %a, ty is %a @]@." Print.pp_expr e Print.pp_type ty; 
  match e, ty with
  (* Primitive expressions *)
  |  Prim (ep), tp ->
     debug dp "primTInt :@\n@[%a, %a@]@." Print.pp_expr e Print.pp_type tp  ; 
      begin
      match ep with
      | PrimInt x -> let ty_ep = Syntax.type_of_prim ep in             
            (* 0 , UINT(D) *)
            begin
              match ty_ep, tp with
              | Ty_IntIndex x, Ty_IntIndex y ->  Tycheck.check_size_eq x y  Tycheck.return_leaf_ch
              | _,  Ty_Prim Ty_PrimInt -> Tycheck.return_leaf_ch
              | _,_ -> fail i @@ WrongShape (tp, "int[i] or int")
            end
         
      | _ -> if tp = Syntax.type_of_prim ep
             then Tycheck.return_leaf_ch else fail i @@ WrongShape (tp, "primitive2")
       end
  | Fix( f, x, ty_x, e), _ -> check_fix f x ty_x e ty
  (* List type expressions *)
  | Nil, _ -> check_nil i ty
  | Cons( e1, e2), _ -> check_cons e1 e2 i ty

  | Case(e, x_l, e_l, x_r, e_r), _ -> check_case_sum i e x_l e_l x_r e_r ty
  (* If statement *)
  | If( e, el, er), _ -> debug dp "checkif, el is %a, ty is %a " Print.pp_expr el Print.pp_type ty ; check_if i e el er ty
  (* Pairs *)
  | Pair(e1, e2), _ ->
    begin
      match ty with
      | Ty_Prod (ty1,ty2) -> (checkType e1 ty1) >> (checkType e2 ty2)
      | _ -> fail i (WrongShape (ty, "product"))
    end
  (* Index abstraction *)
  | ILam (e), Ty_Forall(x, s, mu, k, ty) ->
     if (s = Loc) then
       begin
        check_body ((x |::::| s) i
                      (with_mode mu (checkType e ty))) k
          end
   else    check_body ((x |::| s) i
                  (with_mode mu (checkType e ty))) k
  (* Existential introduction and elimination *)
  | Pack (e), _ -> check_pack i e ty
  | Unpack (e1, vi_x, e2), _ -> check_unpack i e1 vi_x e2 ty
  (* Let bound *)
  | Let (vi_x, e1, e2), _ ->
    inferType e1 <->=
    (fun ty_x -> (vi_x  |:| ty_x) (checkType e2 ty))
  | IApp (e), ty -> 
  | Bernoulli (v) 
  | Uniform (v1, v2)
  | Mech (e)
  | Fst (e)
  | Snd (e)
  | True
  | False


and check_fix (i: info) (vi_f : var_info) (vi_x : var_info) (e : expr) (ty : ty) =
  match ty with
  | Ty_Arrow(ty1, mu, k_fun, ty2) ->
  debug dp "ck_fix:@\n@[e %a, ty1: %a, ty2: %a @]@.@\n"  Print.pp_expr e Print.pp_type ty1 Print.pp_type ty2;
    check_body ((vi_f |:| ty)
                  ((vi_x |:| ty1)
                     (with_mode mu (checkType e ty2)))) k_fun
  | _ ->  fail i (WrongUShape (ty, "fuction"))

and check_body (m: constr checker) (k_fun : iterm) : constr checker =
  fun(ctx, k) ->
    match k with
    | Some k' -> 
        debug dp "body_test:@\n@[k is %a, k_m is %a @]@.@\n"  Print.pp_iterm k' Print.pp_iterm k_fun ;
      begin
        match m (ctx, Some k_fun) with
        | Right c ->  Right (CAnd(c, cost_cs_st ctx (IZero, k')))
        | Left err -> Left err
      end
    | None -> m (ctx, None)

and check_if (i : info) e el er ty =
  debug dp "ck_if:@\n@[e is %a @]@.@\n"  Print.pp_expr e  ;
  inferType e <->=
  (fun ty_g ->
     match ty_g with
     | Ty_Prim UPrimBool -> (checkType el ty) >&&> (checkType er ty)
     | _ -> fail i (WrongUShape (ty, "bool")))

and check_nil i ty =
  match ty with
  | Ty_List(n, ty) -> check_size_eq n IZero return_leaf_ch
  | _ -> fail i (WrongUShape (ty, "list"))


and check_cons e1 e2 i ty =
  match ty with
  | Ty_List(n, ty) ->
    checkType e1 ty >>
    (* Introduce a new size variable and add it to the existential ctx*)
    let v = fresh_evar Size in
    let sz = IVar v in
    (v |:::| Size) i
      (* Check that (sz + 1 = n) *)
      (check_size_eq (n) (ISucc sz)
         (checkType e2 (Ty_List(sz, ty))))
  | _ -> fail i (WrongUShape (ty, "list"))


and check_case_list i e e_n x_h x_tl e_c ty =
  inferType e <->=
  (fun ty_g ->
     match ty_g with
     | Ty_List (n, tye) ->
       (* Nil case *)
       (assume_size_eq n (IConst 0) (checkType e_n ty))
       >&&>
       (* Cons case *)
       (* Generate a fesh size variable *)
       let v = fresh_ivar Size in
       let sz = IVar v in
       (* Extend the index ctx with freshly gen. size*)
       (v |::| Size) i
         ((x_h |:| tye)
            (* Assume that n = sz + 1*)
            (assume_size_eq n (ISucc sz)
               ( (x_tl |:| Ty_List(sz, tye)) (checkType e_c ty))))
     | _ -> fail i (WrongUShape (ty, "list"))
  )

and check_case_sum i e x_l e_l x_r e_r  ty =
  inferType e <->=
  (fun ty_g ->
     match ty_g with
     | Ty_Sum (tyl, tyr) -> 
       ((x_l |:| tyl) (checkType e_l ty)) >>>=
       fun c1 -> (x_r |:| tyr) (checkType e_r ty) >>>=
       fun c2 -> return_ch (merge_cs c1 c2) 
     | _ -> fail i (WrongUShape (ty, "sum"))
  )

and check_pack i e ty =
  match ty with
  | Ty_Exists(x, s, ty) ->
    let v = fresh_evar s in
    let witness = IVar v in
    (v |:::| s) i (checkType e (un_ty_subst x witness ty))
  | _ -> fail i (WrongUShape (ty, "existential"))

and check_unpack i e1 vi_x e2 ty =
  inferType e1 <->=
  (fun ty_x ->
     match ty_x with
     | Ty_Exists(x, s, ty) ->
       (x |::| s) i ((vi_x |:| ty) (checkType e2 ty))
     | _ -> fail i (WrongUShape (ty, "existential")))

(*and check_clet i vi_x e1 e2 ty =
  inferType e1 <->=
  (fun csty ->
     match csty with
     | Ty_Cs(cs, ty_x) ->
        (vi_x |:| ty_x) (checkType e2 ty) >>= fun cs_b -> return_ch (CImpl(cs, cs_b))
     | _ -> fail i (WrongUShape (ty, "constrained")))
*)

and check_dmap dmap dmap' =
  ()


and infer_and_check (i: info) (e: expr) (ty : ty) : constr checker =
  fun(ctx, k) ->
    match inferType e ctx with
    | Right (inf_ty, c, psi_ctx, k') ->
      debug dp "infer_and_check :@\n@[infer_type is %a, expr is %a, checked type is %a, idx_ctx is :%a , k' is %a, k is %a @]@." 
      Print.pp_type inf_ty Print.pp_expr e Print.pp_type ty Print.pp_ivar_ctx ctx.ivar_ctx Print.pp_cost k' Print.pp_cost k; 
      (match (check_subtype i inf_ty ty (extend_e_ctx psi_ctx ctx, None)) with
       | Right c' -> 
	  let cs = option_combine k' k (cost_cs ctx) |> Core.Option.value ~default:CTrue in
    debug dp "infer_and_check2 :@\n@[cs is %a, c is %a, c' is %a @]@." 
      Print.pp_cs cs Print.pp_cs c Print.pp_cs c' ; 
          Right (quantify_all_exist psi_ctx (merge_cs (merge_cs c c') cs))
       | Left err -> Left err)
    | Left err' -> Left err'


let check_type ctx program ty  =
  match checkType program ty ctx with
  | Right c ->  c
  | Left err -> typing_error_pp  err.i pp_tyerr err.v


 
