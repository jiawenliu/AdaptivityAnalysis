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
unary_debug dp "if_TP:@\n@[e1 %a @]@.@\n"  Print.pp_expr e;
  match e with
  | Var ( vi) -> (get_var_ty vi <<= fun ty ->  (return_inf ty))
  | Prim (ep) -> return_inf(un_type_of_prim ep )
  | Fst(e) -> inferType e <<= infer_proj i fst
  | Snd(e) -> inferType e <<= infer_proj i snd
  | App (e1, e2) -> unary_debug dp "if_app:@\n@[e1 %a @]@.@\n"  Print.pp_expr e1 ;
       infer_app (inferType e1) i e2
  | IApp (e) -> infer_iapp (inferType e) i
  |  _ -> fail (expInfo e) (Internal ("no inference rule, try annotating the expression please."))

(*and infer_celim m i = 
   fun ctx -> 
      match m ctx with
    | Right (ty, c, psi, k) ->
      begin
        match ty with
        | Ty_CsImp(c_1,ty_1) ->
            unary_debug dp "celim : k is @[ %a, c_1: %a, c : %a , @] @. " Print.pp_cost k Print.pp_cs c_1 Print.pp_cs c;
            Right ( ty_1 , (CImpl (c_1, c) ) ,  psi, k)
        | _ -> fail i (WrongUShape (ty, "celim fails ")) ctx
      end
    | Left err -> Left err
*)
and infer_check_anno i e ty k =
  fun ctx ->
    match checkType e ty (ctx, Some k) with
    | Right c -> Right (ty, c, [], Some k)
    | Left err -> Left err

and infer_iapp m i =
  fun ctx ->
    match m ctx with
    | Right (ty, c, psi, k) ->
    unary_debug dp "inf_iapp2 :@\n@[c is :%a, ty  %a @]@." Print.pp_cs c Print.pp_type ty ; 
      begin
        match ty with
        | Ty_Forall(x, s, mu', k_e, ty) ->
          let v = fresh_evar s in
          let witn = IVar v in
          let mu = ctx.exec_mode in
          let k' = Core.Option.map ~f:(fun k -> add_costs (k,iterm_subst x witn k_e)) k in 
          if mu = mu'
          then
            Right (un_ty_subst x witn ty, c, (v,s):: psi, k')
          else fail i (WrongMode (mu, mu')) ctx
        | _ -> fail i (WrongUShape (ty, "index quantified (forall) ")) ctx
      end
    | Left err -> Left err

and check_mode i mu (m : constr checker) : constr checker =
  fun(ctx, k) ->
    let mu' = ctx.exec_mode in
    if mu = mu'
    then m(ctx, k)
    else fail i (WrongMode (mu, mu')) (ctx,k)


and infer_app m i e2 =
  m =<-> (fun ty _ ->
      match ty with
      | Ty_Arrow(ty1, mu', dmap, k'', ty2) ->
         unary_debug dp "inf_app :@\n@[ty is :%a, k'' is %a, e2 is %a @]@." Print.pp_type ty Print.pp_iterm k'' Print.pp_expr e2; 
        [(check_mode i mu' (checkType e2 ty1), ty2, ISucc k'')]
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
    unary_debug dp "@[UCK  %a, ty is %a @]@." Print.pp_expr e Print.pp_type ty; 
  match e, ty with
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
  | Fix( f, x, ty_x, e), _ -> check_fix f x ty_x e ty
  (* List type expressions *)
  | Nil, _ -> check_nil i ty
  | Cons( e1, e2), _ -> check_cons e1 e2 i ty

  | Case(e, x_l, e_l, x_r, e_r), _ -> check_case_sum i e x_l e_l x_r e_r ty
  (* If statement *)
  | If( e, el, er), _ -> unary_debug dp "checkif, el is %a, ty is %a " Print.pp_expr el Print.pp_type ty ; check_if i e el er ty
  (* Pairs *)
  | Pair(e1, e2), _ ->
    begin
      match ty with
      | Ty_Prod (ty1,ty2) -> (checkType e1 ty1) >> (checkType e2 ty2)
      | _ -> fail i (WrongUShape (ty, "product"))
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


and check_update (i: info) (e1: expr) (e2 : expr)(e3 : expr) (p:predicate) (k_m: iterm)=
  fun (ctx, k) ->
    match (inferType e1  ctx) with
    | Right (ty_1, c_1, psi_1, k_1) ->
       begin
         match ty_1  with
         | UArray (g, l, ty) ->
            if (List.mem_assoc g p) then
              begin
                match (inferType e2  ctx) with
                | Right (ty_2, c_2, psi_2, k_2) ->
                 unary_debug dp "update_test star :@\n@[psi_2 %a, c_2 is %a @]@."  Print.pp_ivar_ctx psi_2 Print.pp_cs c_2 ;
                   begin
                     match ty_2 with
                     | UInt (l') -> if  (handle_u_update p g l' l ctx i psi_2 c_2) then
                                      begin
                                        let k_sum = option_combine k_1 k_2 (fun (ik,k') -> add_costs( add_costs (ik, ISucc (IZero) ), k') ) in
                                        let k_3 = option_combine (Some k_m) k_sum (fun(ik, k') -> IMinus(ik, k')  ) in
                                        unary_debug dp "update_test :@\n@[k is %a, k_sum is %a, k_3 is %a, e3 is %a @]@." Print.pp_cost k Print.pp_cost k_sum Print.pp_cost k_3 Print.pp_expr e3;
                                        match (checkType e3 ty (ctx, k_3)) with
                                        | Right (c_3) -> Right (      quantify_all_exist (psi_1@psi_2) (merge_cs c_1 (merge_cs c_2 c_3) ) )
                                        | Left err'' -> Left err''
                                      end
                                    
                                    else fail i (WrongUShape (ty_2, " i' not in beta or i' out of bound")) ctx
                     | _ -> fail i (WrongUShape (ty_2, "update second premise")) ctx
                   end
                  
                | Left err' -> Left err'
              end
            else fail i (WrongUShape (ty_1, "update gamma not in predicate P")) ctx
            
         | _ ->  fail i (WrongUShape (ty_1, "update first premise fails")) ctx
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
                 | Right (ty_1, c_1, psi_1, k_1) ->
                    begin
                       unary_debug dp "read first premise :@\n@[type is %a, expr is %a, predicate is %a @]@."  Print.pp_type ty_1 Print.pp_expr e1 Print.pp_predicates p;
                         match ty_1 with
                         | UArray (g, l, ty) ->
                          
                            if (List.mem_assoc g p) then
                            begin
                              match (inferType e2 ctx) with
                              | Right (ty_2, c_2, psi_2, k_2) ->
                                                 begin
                                                   let k_sum =  option_combine k_1 k_2 (fun (ik,k') -> add_costs( add_costs (ik, ISucc(IZero)), k')) in

                                                   let k_leq =  option_combine k_sum (Some k_m) (fun (ik,k') -> cost_cs ctx(ik, k')) in 
                          unary_debug dp "read_test:@\n@[k_sum is %a, k_m is %a, c2 is  %a, k_2 is %a @]@.@\n" 
                             Print.pp_cost k_sum Print.pp_iterm k_m Print.pp_cs c_2 Print.pp_cost k_2  ;
                                                   let k_cs = Core.Option.value_map ~default:CTrue
      				                                ~f:(fun k -> k) k_leq in
                                                 
                                                   match ty_2 with
                                                    | UInt (l') ->  Right ( (* merge_cs *) ( quantify_all_exist  (psi_1@psi_2) (merge_cs k_cs     (merge_cs c_1 c_2)  ) )    (* (CLeq(l',l) *) ) 
                                                    | _  ->   fail i (WrongUShape (ty, "read second premise fails")) ctx
                                                 end
                            | Left err' -> Left err'
                           end
                            else fail i (WrongUShape (ty_1, "gamma not in predicate read")) ctx
                         | _ -> fail i (WrongUShape (ty_1, "read first premise fails")) ctx
                       end 
                 | Left err -> Left err
   


and check_fix (i: info) (vi_f : var_info) (vi_x : var_info) (e : expr) (ty : ty) =
  match ty with
  | Ty_Arrow(ty1, mu, k_fun, ty2) ->
  unary_debug dp "ck_fix:@\n@[e %a, ty1: %a, ty2: %a @]@.@\n"  Print.pp_expr e Print.pp_type ty1 Print.pp_type ty2;
    check_body ((vi_f |:| ty)
                  ((vi_x |:| ty1)
                     (with_mode mu (checkType e ty2)))) k_fun
  | _ ->  fail i (WrongUShape (ty, "fuction"))

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

and check_if (i : info) e el er ty =
  unary_debug dp "ck_if:@\n@[e is %a @]@.@\n"  Print.pp_expr e  ;
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
      unary_debug dp "infer_and_check :@\n@[infer_type is %a, expr is %a, checked type is %a, idx_ctx is :%a , k' is %a, k is %a @]@." 
      Print.pp_type inf_ty Print.pp_expr e Print.pp_type ty Print.pp_ivar_ctx ctx.ivar_ctx Print.pp_cost k' Print.pp_cost k; 
      (match (check_subtype i inf_ty ty (extend_e_ctx psi_ctx ctx, None)) with
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


 
