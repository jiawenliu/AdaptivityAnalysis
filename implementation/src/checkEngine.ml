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
let unary_debug   fi = Support.Error.message 4 General fi

let typing_error fi = Err.error_msg    Opt.TypeChecker fi
let typing_error_pp = Err.error_msg_pp Opt.TypeChecker


(** Check whether uty1 is a subtype of uty2, generating the necessary
    constraints along the way. **)
let rec check_subtype (i: info) (ty1 : ty) (ty2 : ty) : constr checker =
  let fail = fail i @@ NotSubtype (uty1, uty2) in
  	if ty1 = ty2 then return_ch empty_constr
  	else 
  match ty1, ty2 with
  | Ty_Box bty1, bty2 -> 
  if bty1 = bty2 then return_ch empty_constr
  else
  begin
  match bty1, bty2 with
  	| 
end
  | bty1, Ty_Box bty2 -> 
  begin
  match bty1, bty2 with
  	| Ty_Prim pty1, Ty_Prim pty2 -> 
  		if pty1 = pty2 then return_ch empty_constr
  					   else fail
  	| 
end (* check the subtype for box type*)return_ch empty_constr

  | _, _ -> fail


                 
(** [inferType e] infers that expression [e] has type [un_ty] along
    with cost [k] in context [ctx]. If it does, it returns [un_ty
    inferer: (un_ty, k, psi)] where all the free existential variables
    occuring in un_ty and k are declared in psi, otherwise it raises
    an exception. *)
let rec inferType (e: expr) : ty inferer  =
unary_debug dp "if_TP:@\n@[e1 %a @]@.@\n"  Print.pp_expr e;
  match e with
  | Var (i, vi) -> (get_var_ty i vi <<= fun ty ->  (return_inf ty))
  | Prim (i, ep) -> return_inf(un_type_of_prim ep )
  | Fst(i, e) -> inferType e <<= infer_proj i fst
  | Snd(i, e) -> inferType e <<= infer_proj i snd
  | App (i, e1, e2) -> unary_debug dp "if_app:@\n@[e1 %a @]@.@\n"  Print.pp_expr e1 ;
       infer_app (inferType e1) i e2
  | IApp (i, e) -> infer_iapp (inferType e) i
  | UAnno (i, e, uty, k) -> unary_debug dp "if_UAnno:@\n@[e1 %a @]@.@\n"  Print.pp_expr e; infer_check_anno i e uty k
  | CExpr (i, e) ->  unary_debug dp "inf_celim :@\n@[e is %a @]@."  Print.pp_expr e; infer_celim (inferType e) i 
  |  _ -> fail (expInfo e) (Internal ("no inference rule, try annotating the expression please."))

and infer_celim m i = 
   fun ctx -> 
      match m ctx with
    | Right (uty, c, psi, k) ->
      begin
        match uty with
        | UTyCsImp(c_1,uty_1) ->
            unary_debug dp "celim : k is @[ %a, c_1: %a, c : %a , @] @. " Print.pp_cost k Print.pp_cs c_1 Print.pp_cs c;
            Right ( uty_1 , (CImpl (c_1, c) ) ,  psi, k)
        | _ -> fail i (WrongUShape (uty, "celim fails ")) ctx
      end
    | Left err -> Left err

and infer_check_anno i e uty k =
  fun ctx ->
    match checkType e uty (ctx, Some k) with
    | Right c -> Right (uty, c, [], Some k)
    | Left err -> Left err

and infer_iapp m i =
  fun ctx ->
    match m ctx with
    | Right (uty, c, psi, k) ->
    unary_debug dp "inf_iapp2 :@\n@[c is :%a, uty  %a @]@." Print.pp_cs c Print.pp_type uty ; 
      begin
        match uty with
        | UTyForall(x, s, mu', k_e, ty) ->
          let v = fresh_evar s in
          let witn = IVar v in
          let mu = ctx.exec_mode in
          let k' = Core.Option.map ~f:(fun k -> add_costs (k,iterm_subst x witn k_e)) k in 
          if mu = mu'
          then
            Right (un_ty_subst x witn ty, c, (v,s):: psi, k')
          else fail i (WrongMode (mu, mu')) ctx
        | _ -> fail i (WrongUShape (uty, "index quantified (forall) ")) ctx
      end
    | Left err -> Left err

and check_mode i mu (m : constr checker) : constr checker =
  fun(ctx, k) ->
    let mu' = ctx.exec_mode in
    if mu = mu'
    then m(ctx, k)
    else fail i (WrongMode (mu, mu')) (ctx,k)


and infer_app m i e2 =
  m =<-> (fun uty _ ->
      match uty with
      | UTyArr(ty1, mu', k'', ty2) ->
         unary_debug dp "inf_app :@\n@[uty is :%a, k'' is %a, e2 is %a @]@." Print.pp_type uty Print.pp_iterm k'' Print.pp_expr e2; 
        [(check_mode i mu' (checkType e2 ty1), ty2, ISucc k'')]
      | _ -> [(fail i (WrongUShape (uty, "function")), UTyPrim (UPrimInt), IZero)])

and infer_proj i f =
  fun uty ->
    match uty with
    | UTyProd (ty1, ty2) ->  return_inf(f (ty1, ty2))
    | _ -> fail i (WrongUShape (uty, "product"))

(** [checkType e] verifies that expression [e] has unary type [uty]
    in the context [ctx] with the cost [k]. If
    it does, it returns unit, otherwise it raises an exception. *)
and checkType (e: expr) (uty : ty) : constr checker =
    unary_debug dp "@[UCK  %a, uty is %a @]@." Print.pp_expr e Print.pp_type uty; 
  match e, uty with
  (* Primitive expressions *)
  |  Prim (ep), tp ->
     unary_debug dp "primTInt :@\n@[%a, %a@]@." Print.pp_expr e Print.pp_type tp  ; 
      begin
      match ep with
      | PrimInt x -> let ty_ep = Syntax.type_of_prim ep in             
            (* 0 , UINT(D) *)
            begin
              match ty_ep, tp with
              | Ty_IntIndex x, Ty_IntIndex y ->  Tycheck.check_size_eq x y  Tycheck.return_leaf_ch
              | _,  Ty_Prim Ty_PrimInt -> Tycheck.return_leaf_ch
              | _,_ -> fail i @@ WrongUShape (tp, "int[i] or int")
            end
         
      | _ -> if tp = Syntax.type_of_prim ep
             then Tycheck.return_leaf_ch else fail i @@ WrongUShape (tp, "primitive2")
       end
  | Fix( vi_f, vi_x, e), _ -> check_fix i vi_f vi_x e uty
  (* List type expressions *)
  | Nil, _ -> check_nil i uty
  | Cons( e1, e2), _ -> check_cons e1 e2 i uty

  | Case(e, x_l, e_l, x_r, e_r), _ -> check_case_sum i e x_l e_l x_r e_r uty
  (* If statement *)
  | If( e, el, er), _ -> unary_debug dp "checkif, el is %a, uty is %a " Print.pp_expr el Print.pp_type uty ; check_if i e el er uty
  (* Pairs *)
  | Pair(e1, e2), _ ->
    begin
      match uty with
      | UTyProd (ty1,ty2) -> (checkType e1 ty1) >> (checkType e2 ty2)
      | _ -> fail i (WrongUShape (uty, "product"))
    end
  (* Index abstraction *)
  | ILam (e), UTyForall(x, s, mu, k, ty) ->
     if (s = Loc) then
       begin
        check_body ((x |::::| s) i
                      (with_mode mu (checkType e ty))) k
          end
   else    check_body ((x |::| s) i
                  (with_mode mu (checkType e ty))) k
  (* Existential introduction and elimination *)
  | Pack (e), _ -> check_pack i e uty
  | Unpack (e1, vi_x, e2), _ -> check_unpack i e1 vi_x e2 uty
  (* Let bound *)
  | Let (vi_x, e1, e2), _ ->
    inferType e1 <->=
    (fun uty_x -> (vi_x  |:| uty_x) (checkType e2 uty))
  | IApp (e), ty -> 
  | Bernoulli (v) 
  | Uniform (v1, v2)
  | Mech (e)
  | Fst (e)
  | Snd (e)
  | True
  | False


and check_update (i: info) (e1: expr) (e2 : expr)(e3 : expr) (p:predicate) (k_m: iterm)=
  fun (ctx, k) ->
    match (inferType e1  ctx) with
    | Right (uty_1, c_1, psi_1, k_1) ->
       begin
         match uty_1  with
         | UArray (g, l, uty) ->
            if (List.mem_assoc g p) then
              begin
                match (inferType e2  ctx) with
                | Right (uty_2, c_2, psi_2, k_2) ->
                 unary_debug dp "update_test star :@\n@[psi_2 %a, c_2 is %a @]@."  Print.pp_ivar_ctx psi_2 Print.pp_cs c_2 ;
                   begin
                     match uty_2 with
                     | UInt (l') -> if  (handle_u_update p g l' l ctx i psi_2 c_2) then
                                      begin
                                        let k_sum = option_combine k_1 k_2 (fun (ik,k') -> add_costs( add_costs (ik, ISucc (IZero) ), k') ) in
                                        let k_3 = option_combine (Some k_m) k_sum (fun(ik, k') -> IMinus(ik, k')  ) in
                                        unary_debug dp "update_test :@\n@[k is %a, k_sum is %a, k_3 is %a, e3 is %a @]@." Print.pp_cost k Print.pp_cost k_sum Print.pp_cost k_3 Print.pp_expr e3;
                                        match (checkType e3 uty (ctx, k_3)) with
                                        | Right (c_3) -> Right (      quantify_all_exist (psi_1@psi_2) (merge_cs c_1 (merge_cs c_2 c_3) ) )
                                        | Left err'' -> Left err''
                                      end
                                    
                                    else fail i (WrongUShape (uty_2, " i' not in beta or i' out of bound")) ctx
                     | _ -> fail i (WrongUShape (uty_2, "update second premise")) ctx
                   end
                  
                | Left err' -> Left err'
              end
            else fail i (WrongUShape (uty_1, "update gamma not in predicate P")) ctx
            
         | _ ->  fail i (WrongUShape (uty_1, "update first premise fails")) ctx
       end
   
        | Left err -> Left err

and   handle_u_update (p:predicate) (g: var_info) (l': iterm) (l: iterm) (ctx: ty context) (i:info) (psi_2) c_2': bool =
     unary_debug dp "handler_u_updte1 : @[ l':%a, l: %a, cs_env: %a, idx_env is %a , evar_ctx is: %a @]" 
      Print.pp_iterm l' Print.pp_iterm l Print.pp_cs ctx.constr_env Print.pp_ivar_ctx ctx.ivar_ctx Print.pp_ivar_ctx ctx.evar_ctx ;
          let beta = (List.assoc_opt g p) in
            match beta with
             | None -> false
             | Some (IBeta b) ->
                 let c_1 =  CBetaIn (  l', b )  in
                   (*  let c_2 =  merge_cs c_1  (CLeq (l', l)) in  *)
                    let c_2 = merge_cs c_2' (merge_cs  (CLeq (l', l)) c_1 )in 
                      let c_3 =  CImpl (ctx.constr_env  , c_2) in 
                        let c_4 = quantify_all_universal ctx.ivar_ctx c_3  in 
                          let c_5 = quantify_all_exist (ctx.evar_ctx @ psi_2) c_4 in
                           unary_debug dp "handler_u_updte4 : @[ c_5: %a @]"   Print.pp_cs c_5;
                            let elim_c5 =  Exist_elim.elimt ctx c_5  
                            (fun cs' ->
                               cs'
                              ) in 
                            unary_debug dp "handler_u_updte5 : @[ elim_c_5: %a @]"   Print.pp_cs elim_c5;
                            let w_c= WhyTrans.why3_translate_int elim_c5 in 
                            unary_debug dp "handler_u_updte3 : @[  w_c: %a, c_4: %a @]"  Why3.Pretty.print_term w_c Print.pp_cs c_5;
                              WhySolver.post_st w_c 1
             | _ -> false
                    
and check_read (i: info) (e1: expr) (e2 : expr) (p:predicate) (k_m: iterm)  =
  fun (ctx,k) -> match (inferType e1  ctx) with
                 | Right (uty_1, c_1, psi_1, k_1) ->
                    begin
                       unary_debug dp "read first premise :@\n@[type is %a, expr is %a, predicate is %a @]@."  Print.pp_type uty_1 Print.pp_expr e1 Print.pp_predicates p;
                         match uty_1 with
                         | UArray (g, l, uty) ->
                          
                            if (List.mem_assoc g p) then
                            begin
                              match (inferType e2 ctx) with
                              | Right (uty_2, c_2, psi_2, k_2) ->
                                                 begin
                                                   let k_sum =  option_combine k_1 k_2 (fun (ik,k') -> add_costs( add_costs (ik, ISucc(IZero)), k')) in

                                                   let k_leq =  option_combine k_sum (Some k_m) (fun (ik,k') -> cost_cs ctx(ik, k')) in 
                          unary_debug dp "read_test:@\n@[k_sum is %a, k_m is %a, c2 is  %a, k_2 is %a @]@.@\n" 
                             Print.pp_cost k_sum Print.pp_iterm k_m Print.pp_cs c_2 Print.pp_cost k_2  ;
                                                   let k_cs = Core.Option.value_map ~default:CTrue
      				                                ~f:(fun k -> k) k_leq in
                                                 
                                                   match uty_2 with
                                                    | UInt (l') ->  Right ( (* merge_cs *) ( quantify_all_exist  (psi_1@psi_2) (merge_cs k_cs     (merge_cs c_1 c_2)  ) )    (* (CLeq(l',l) *) ) 
                                                    | _  ->   fail i (WrongUShape (uty, "read second premise fails")) ctx
                                                 end
                            | Left err' -> Left err'
                           end
                            else fail i (WrongUShape (uty_1, "gamma not in predicate read")) ctx
                         | _ -> fail i (WrongUShape (uty_1, "read first premise fails")) ctx
                       end 
                 | Left err -> Left err
   


and check_fix (i: info) (vi_f : var_info) (vi_x : var_info) (e : expr) (uty : ty) =
  match uty with
  | UTyArr(ty1, mu, k_fun, ty2) ->
  unary_debug dp "ck_fix:@\n@[e %a, ty1: %a, ty2: %a @]@.@\n"  Print.pp_expr e Print.pp_type ty1 Print.pp_type ty2;
    check_body ((vi_f |:| uty)
                  ((vi_x |:| ty1)
                     (with_mode mu (checkType e ty2)))) k_fun
  | _ ->  fail i (WrongUShape (uty, "fuction"))

and check_body (m: constr checker) (k_fun : iterm) : constr checker =
  fun(ctx, k) ->
    match k with
    | Some k' -> 
        unary_debug dp "body_test:@\n@[k is %a, k_m is %a @]@.@\n"  Print.pp_iterm k' Print.pp_iterm k_fun ;
      begin
        match m (ctx, Some k_fun) with
        | Right c ->  Right (CAnd(c, cost_cs_st ctx (IZero, k')))
        | Left err -> Left err
      end
    | None -> m (ctx, None)

and check_if (i : info) e el er uty =
  unary_debug dp "ck_if:@\n@[e is %a @]@.@\n"  Print.pp_expr e  ;
  inferType e <->=
  (fun uty_g ->
     match uty_g with
     | UTyPrim UPrimBool -> (checkType el uty) >&&> (checkType er uty)
     | _ -> fail i (WrongUShape (uty, "bool")))

and check_nil i uty =
  match uty with
  | UTyList(n, ty) -> check_size_eq n IZero return_leaf_ch
  | _ -> fail i (WrongUShape (uty, "list"))


and check_cons e1 e2 i uty =
  match uty with
  | UTyList(n, ty) ->
    checkType e1 ty >>
    (* Introduce a new size variable and add it to the existential ctx*)
    let v = fresh_evar Size in
    let sz = IVar v in
    (v |:::| Size) i
      (* Check that (sz + 1 = n) *)
      (check_size_eq (n) (ISucc sz)
         (checkType e2 (UTyList(sz, ty))))
  | _ -> fail i (WrongUShape (uty, "list"))


and check_case_list i e e_n x_h x_tl e_c uty =
  inferType e <->=
  (fun uty_g ->
     match uty_g with
     | UTyList (n, tye) ->
       (* Nil case *)
       (assume_size_eq n IZero (checkType e_n uty))
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
               ( (x_tl |:| UTyList(sz, tye)) (checkType e_c uty))))
     | _ -> fail i (WrongUShape (uty, "list"))
  )

and check_case_sum i e x_l e_l x_r e_r  uty =
  inferType e <->=
  (fun uty_g ->
     match uty_g with
     | UTySum (tyl, tyr) -> 
       ((x_l |:| tyl) (checkType e_l uty)) >>>=
       fun c1 -> (x_r |:| tyr) (checkType e_r uty) >>>=
       fun c2 -> return_ch (merge_cs c1 c2) 
     | _ -> fail i (WrongUShape (uty, "sum"))
  )

and check_pack i e uty =
  match uty with
  | UTyExists(x, s, ty) ->
    let v = fresh_evar s in
    let witness = IVar v in
    (v |:::| s) i (checkType e (un_ty_subst x witness ty))
  | _ -> fail i (WrongUShape (uty, "existential"))

and check_unpack i e1 vi_x e2 uty =
  inferType e1 <->=
  (fun uty_x ->
     match uty_x with
     | UTyExists(x, s, ty) ->
       (x |::| s) i ((vi_x |:| ty) (checkType e2 uty))
     | _ -> fail i (WrongUShape (uty, "existential")))

and check_clet i vi_x e1 e2 uty =
  inferType e1 <->=
  (fun csty ->
     match csty with
     | UTyCs(cs, uty_x) ->
        (vi_x |:| uty_x) (checkType e2 uty) >>= fun cs_b -> return_ch (CImpl(cs, cs_b))
     | _ -> fail i (WrongUShape (uty, "constrained")))




and infer_and_check (i: info) (e: expr) (uty : ty) : constr checker =
  fun(ctx, k) ->
    match inferType e ctx with
    | Right (inf_uty, c, psi_ctx, k') ->
      unary_debug dp "infer_and_check :@\n@[infer_type is %a, expr is %a, checked type is %a, idx_ctx is :%a , k' is %a, k is %a @]@." 
      Print.pp_type inf_uty Print.pp_expr e Print.pp_type uty Print.pp_ivar_ctx ctx.ivar_ctx Print.pp_cost k' Print.pp_cost k; 
      (match (check_subtype i inf_uty uty (extend_e_ctx psi_ctx ctx, None)) with
       | Right c' -> 
	  let cs = option_combine k' k (cost_cs ctx) |> Core.Option.value ~default:CTrue in
    unary_debug dp "infer_and_check2 :@\n@[cs is %a, c is %a, c' is %a @]@." 
      Print.pp_cs cs Print.pp_cs c Print.pp_cs c' ; 
          Right (quantify_all_exist psi_ctx (merge_cs (merge_cs c c') cs))
       | Left err -> Left err)
    | Left err' -> Left err'


let check_type ctx program ty  =
  match checkType program ty ctx with
  | Right c ->  c
  | Left err -> typing_error_pp  err.i pp_tyerr err.v


 