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

 let  check_iarray_sub (iarr_1: iterm) (iarr_2: iterm)  = 
   match iarr_1, iarr_2 with
      | IBeta b1, IBeta b2 ->  CBetaSub (b1 ,b2)

      |_ -> CTrue



 let ck_sub p1 p2 = 
     List.fold_left ( fun c (g, iarr) ->
             match List.assoc_opt g p2 with 
              | Some (iarr') -> CAnd (c, check_iarray_sub iarr iarr') 
              | None -> CTrue
       ) CTrue  p1    

     let get_fst c =
       match c with
       | CAnd(c1,c2) -> c1 
       | _ -> c

     let check_predicate_sub_cs p1 p2 q1 q2  = 
        let c_1 = ck_sub p1 p2 in 
        let c_2 = ck_sub q1 q2 in
        (* let c= CAnd( get_fst c_1, get_fst c_2) in *)
        let c= CAnd( c_1,  c_2) in
       c  


let  check_beta_eq (b1:beta) (b2:beta) ctx : bool =
    let c_3= CBetaEq (b1 ,b2) in

    let c_4 = quantify_all_exist ctx.evar_ctx c_3 in 
                          let c_5 = quantify_all_universal ctx.ivar_ctx c_4 in
                            let w_c= WhyTrans.why3_translate_int c_5 in 
                           
                              WhySolver.post_st w_c 1

let  check_beta_sub (b1:beta) (b2:beta) ctx : bool =
    
    let c_2= CBetaSub (b1 ,b2) in
    let c_3 =  CImpl (ctx.constr_env, c_2) in 
    let c_4 = quantify_all_exist ctx.evar_ctx c_3 in 
                          let c_5 = quantify_all_universal ctx.ivar_ctx c_4 in
                            let w_c= WhyTrans.why3_translate_int c_5 in 
                           
                              WhySolver.post_st w_c 1




let  check_iarrays_eq (iarr_1: iterm) (iarr_2: iterm) ctx : bool = 
     match iarr_1, iarr_2 with
      | IBeta b1, IBeta b2 ->  check_beta_eq b1 b2 ctx

      |_ -> false

      (* match iarr_1 , iarr_2 with
        | IArray (o_1, l_1 ), IArray (o_2,l_2) ->
            let l_1' = List.sort sort_alg l_1 in 
            let l_2' = List.sort sort_alg l_2 in
            (l_1' = l_2') 
        | IVar v1, IVar v2 ->  v2.v_name = v1.v_name
        | IArrUnion (a_1, s_1, e_1), IArrUnion (a_2, s_2,e_2)  -> (check_iarrays_eq a_1 a_2) && s_1 = s_2 && e_1 = e_2  
        | _ , _ -> false      *) 
   
let  check_iarrays_sub (iarr_1: iterm) (iarr_2: iterm)  ctx: bool = 
   match iarr_1, iarr_2 with
      | IBeta b1, IBeta b2 ->  check_beta_sub b1 b2 ctx

      |_ -> false
     (*  match iarr_1 , iarr_2 with
        | IArray (o_1, l_1 ), IArray (o_2,l_2) ->
            let l_1' = List.sort sort_alg l_1 in 
            let l_2' = List.sort sort_alg l_2 in
            (l_1' = l_2') 
        | Ivar v1, Ivar v2 ->  v2.v_name = v1.v_name
        | IArrUnion (a_1, s_1, e_1), IArrUnion (a_2, s_2,e_2)  -> (check_iarrays_eq a_1 a_2) && s_1 = s_2 && e_1 = e_2  
        | _ , _ -> false    
 *)

   let check_predicate_sub (p : predicate) (q :  predicate) ctx: bool =
      if (List.length p) = (List.length q) then 
         List.for_all (fun (g, iarr) ->
             match List.assoc_opt g q with 
              | Some (iarr') -> check_iarrays_sub iarr iarr' ctx
              | None -> false
          ) p
      else false     


  let check_predicate (p : predicate) (q :  predicate) ctx: bool =
      if (List.length p) = (List.length q) then 
         List.for_all (fun (g, iarr) ->
             match List.assoc_opt g q with 
              | Some (iarr') ->  check_iarrays_eq iarr iarr' ctx
              | None -> false
          ) p
      else false


  let preselect_alloc (p: predicate) (q: predicate) (g: var_info) ctx : bool =
    if ( List.mem_assoc g q ) && not (List.mem_assoc g p) && (List.length q = (List.length p) + 1) then
      List.for_all (fun (g,iarr) ->
           match List.assoc_opt g q with 
              | Some (iarr') -> check_iarrays_eq iarr iarr' ctx
              | None -> false
        ) p
    else false

   let preselect_return (p : predicate) (q :  predicate) ctx : bool =
      if (List.length p) = (List.length q) then 
         List.for_all (fun (g, iarr) ->
             match List.assoc_opt g q with 
              | Some (iarr') -> check_iarrays_eq iarr iarr' ctx
              | None -> false
          ) p
      else false

(* Check whether uty1 is a subtype of uty2, generating the necessary constraints along the way. *)
let rec check_usubtype (i: info) (uty1 : un_ty) (uty2 : un_ty) : constr checker =
  let fail = fail i @@ NotUSubtype (uty1, uty2) in
  match uty1, uty2 with
  | UTyPrim pty1, UTyPrim pty2 ->
    if pty1 = pty2 then return_ch empty_constr else fail
  | UTyList (n, ty1), UTyList (n', ty2) -> check_size_eq n n' (check_usubtype i ty1 ty2)
  | UTySum (ty1, ty2), UTySum (ty1',ty2') -> check_usubtype i ty1 ty1' >> check_usubtype i ty2 ty2'
  | UTyProd (ty1, ty2), UTyProd (ty1',ty2') -> check_usubtype i ty1 ty1' >> check_usubtype i ty2 ty2'
  | UTyArr (ty1,mo, k, ty2), UTyArr (ty1',mo', k', ty2') ->
     if mo = mo' then check_size_leq k k' (check_usubtype i ty1' ty1 >> check_usubtype i ty2 ty2')
     else fail
  | UTyForall (x,s, mo, k, ty), UTyForall (x',s', mo', k', ty') ->
     if (mo = mo' && s = s') then
       let m = fresh_evar s in
       let x_m = IVar m in
       (m |::| s) i
          (check_size_leq (iterm_subst x x_m k) (iterm_subst x' x_m k')
              (check_usubtype i (un_ty_subst x x_m ty) (un_ty_subst x' x_m ty')))
     else fail
  | UTyExists (x,s, ty), UTyExists (x',s', ty') ->
     if  x = x' && s = s' then
        (x |::| s) i (check_usubtype i ty ty')
     else fail
  (*monadic*)
  | UInt (x), UInt (x') -> check_size_eq x x' (return_ch empty_constr)
  | UInt (x), UTyPrim  UPrimInt -> return_ch empty_constr
  | UArray (g,x,uty), UArray (g', x', uty') ->  
      if g = g' then 
         check_size_eq x x' (check_usubtype i uty uty')
      else fail
  | UMonad(p_1, g_1,uty_1, k_1, mu_1, q_1), UMonad(p_1', g_1', uty_1', k_1', mu_1', q_1')->
     fun (ctx, k) -> 

    if g_1 = g_1' && mu_1 = mu_1' && (check_predicate_sub p_1 p_1' ctx) && (check_predicate_sub q_1 q_1' ctx) then 
           match mu_1 with
            | MaxEx-> ( check_size_leq k_1 k_1' (check_usubtype i uty_1 uty_1') ) (ctx,k)
            | MinEx -> ( check_size_leq k_1' k_1 (check_usubtype i uty_1 uty_1') ) (ctx,k)
    else fail (ctx,k)
  | _, _ -> fail



(** Check whether uty1 is a subtype of uty2, generating the necessary
    constraints along the way. **)
let rec check_usubtype (i: info) (uty1 : ty) (uty2 : ty) : constr checker =
  let fail = fail i @@ NotUSubtype (uty1, uty2) in
  match uty1, uty2 with
  | UTyPrim pty1, UTyPrim pty2 -> if pty1 = pty2 then return_ch empty_constr
                                  else fail
  | UTyList (n, ty1), UTyList (n', ty2) -> check_size_eq n n' (check_usubtype i ty1 ty2)
  | UTySum (ty1, ty2), UTySum (ty1',ty2') -> check_usubtype i ty1 ty1' >> check_usubtype i ty2 ty2'
  | UTyProd (ty1, ty2), UTyProd (ty1',ty2') -> check_usubtype i ty1 ty1' >> check_usubtype i ty2 ty2'
  | UTyArr (ty1,mo, k, ty2), UTyArr (ty1',mo', k', ty2') ->
     if mo = mo' then check_size_leq k k' (check_usubtype i ty1' ty1 >> check_usubtype i ty2 ty2')
     else fail
  | UTyForall (x,s, mo, k, ty), UTyForall (x',s', mo', k', ty') ->
     if (mo = mo' && s = s') then
       let m = fresh_evar s in
       let x_m = IVar m in
       (m |::| s) i
          (check_size_leq (iterm_subst x x_m k) (iterm_subst x' x_m k')
              (check_usubtype i (un_ty_subst x x_m ty) (un_ty_subst x' x_m ty')))
     else fail
  | UTyExists (x,s, ty), UTyExists (x',s', ty') ->
     if  x = x' && s = s' then
       (x |::| s) i (check_usubtype i ty ty')
     else fail
  | UTyCs(c1, ty1), UTyCs(c2, ty2) -> 
     check_usubtype i ty1 ty2 >>=
       fun cs_b -> return_ch (CAnd(CImpl(c1, c2), cs_b))
  (*monadic*)
  | UInt (x), UInt (x') -> check_size_eq x x' (return_ch empty_constr)
  | UInt (x), UTyPrim  UPrimInt -> return_ch empty_constr
  | UArray (g,x,uty), UArray (g', x', uty') ->  
      if g = g' then 
        check_size_eq x x' (check_usubtype i uty uty')
      else fail
  | UMonad(p_1, g_1,uty_1, k_1, mu_1, q_1), UMonad(p_1', g_1', uty_1', k_1', mu_1', q_1')->
     fun (ctx, k) -> 
      unary_debug dp "UMONAD SUB :@\n@[k is %a, k' is %a @]@."  Print.pp_iterm k_1 Print.pp_iterm k_1';

    if g_1 = g_1' && mu_1 = mu_1' then 

           match mu_1 with
            | MaxEx->
              begin
               match ( check_size_leq k_1 k_1' (check_usubtype i uty_1 uty_1') ) (ctx,k)  with
                  | Right c -> Right (merge_cs c (check_predicate_sub_cs p_1 p_1' q_1 q_1') )
                  | Left err -> Left err
              end
            | MinEx ->
              begin
               match ( check_size_leq k_1' k_1 (check_usubtype i uty_1 uty_1') ) (ctx,k)  with
                  | Right c -> Right (merge_cs c (check_predicate_sub_cs p_1 p_1' q_1 q_1') )
                  | Left err -> Left err
              end
    else fail (ctx,k)

  | _, _ -> fail


                 
(** [inferType e] infers that expression [e] has type [un_ty] along
    with cost [k] in context [ctx]. If it does, it returns [un_ty
    inferer: (un_ty, k, psi)] where all the free existential variables
    occuring in un_ty and k are declared in psi, otherwise it raises
    an exception. *)
let rec inferUType (e: expr) : ty inferer  =
unary_debug dp "if_UTP:@\n@[e1 %a @]@.@\n"  Print.pp_expr e;
  match e with
  | Var (i, vi) -> (get_var_ty i vi <<= fun ty ->  (return_inf ty))
  | Prim (i, ep) -> return_inf(un_type_of_prim ep )
  | Fst(i, e) -> inferUType e <<= infer_proj i fst
  | Snd(i, e) -> inferUType e <<= infer_proj i snd
  | App (i, e1, e2) -> unary_debug dp "if_app:@\n@[e1 %a @]@.@\n"  Print.pp_expr e1 ;
       infer_app (inferUType e1) i e2
  | IApp (i, e) -> infer_iapp (inferUType e) i
  | UAnno (i, e, uty, k) -> unary_debug dp "if_UAnno:@\n@[e1 %a @]@.@\n"  Print.pp_expr e; infer_check_anno i e uty k
  | CExpr (i, e) ->  unary_debug dp "inf_celim :@\n@[e is %a @]@."  Print.pp_expr e; infer_celim (inferUType e) i 
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
    match checkUType e uty (ctx, Some k) with
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
        [(check_mode i mu' (checkUType e2 ty1), ty2, ISucc k'')]
      | _ -> [(fail i (WrongUShape (uty, "function")), UTyPrim (UPrimInt), IZero)])

and infer_proj i f =
  fun uty ->
    match uty with
    | UTyProd (ty1, ty2) ->  return_inf(f (ty1, ty2))
    | _ -> fail i (WrongUShape (uty, "product"))

(** [checkUType e] verifies that expression [e] has unary type [uty]
    in the context [ctx] with the cost [k]. If
    it does, it returns unit, otherwise it raises an exception. *)
and checkUType (e: expr) (uty : ty) : constr checker =
    unary_debug dp "@[UCK  %a, uty is %a @]@." Print.pp_expr e Print.pp_type uty; 
  match e, uty with
  (* Constrained type intro *)
  | _, UTyCs(c, ty) -> 
            (with_new_ctx (fun ctx -> {ctx with constr_env = CAnd (c, ctx.constr_env )} ) (checkUType e ty) ) >>= fun cs_b -> return_ch (CAnd(c, CImpl(c, cs_b)))
  | _, UTyCsImp (c, ty) -> 
  unary_debug dp "@[UTY Cimpl is %a, e is :%a@]@." Print.pp_cs c Print.pp_expr e; 
   (with_new_ctx (fun ctx -> {ctx with constr_env = CAnd (c, ctx.constr_env )} ) (checkUType e ty) ) >>= fun cs_b -> unary_debug dp "@[UTY Cimpl2 CSB is %a@]@." Print.pp_cs cs_b; return_ch (CImpl(c, cs_b))
  (* Primitive expressions *)
  |  Prim (i, ep), tp ->
     unary_debug dp "primTInt :@\n@[%a, %a@]@." Print.pp_expr e Print.pp_type tp  ; 
      begin
      match ep with
      | PrimTInt x -> let ty_ep = un_type_of_prim ep in             
            (* 0 , UINT(D) *)
            begin
              match ty_ep, tp with
              | UInt x, UInt y ->  check_size_eq x y  return_leaf_ch
              | _,  UTyPrim UPrimInt -> return_leaf_ch
              | _,_ -> fail i @@ WrongUShape (tp, "int[i] or int")
            end
         
      | _ -> if tp = un_type_of_prim ep
             then return_leaf_ch else fail i @@ WrongUShape (tp, "primitive2")
       end
  |  Fix(i, vi_f, vi_x, e), _ -> check_fix i vi_f vi_x e uty
  (* List type expressions *)
  | Nil i, _ -> check_nil i uty
  | Cons(i, e1, e2), _ -> check_cons e1 e2 i uty

  | Case(i,e, x_l, e_l, x_r, e_r), _ -> check_case_sum i e x_l e_l x_r e_r uty
  (* If statement *)
  | IfThen(i, e, el, er), _ -> unary_debug dp "checkif, el is %a, uty is %a " Print.pp_expr el Print.pp_type uty ; check_if i e el er uty
  (* Pairs *)
  | Pair(i, e1, e2), _ ->
    begin
      match uty with
      | UTyProd (ty1,ty2) -> (checkUType e1 ty1) >> (checkUType e2 ty2)
      | _ -> fail i (WrongUShape (uty, "product"))
    end
  (* Index abstraction *)
  | ILam (i, e), UTyForall(x, s, mu, k, ty) ->
     if (s = Loc) then
       begin
        check_body ((x |::::| s) i
                      (with_mode mu (checkUType e ty))) k
          end
   else    check_body ((x |::| s) i
                  (with_mode mu (checkUType e ty))) k
  (* Existential introduction and elimination *)
  | Pack (i, e), _ -> check_pack i e uty
  | Unpack (i, e1, vi_x, e2), _ -> check_unpack i e1 vi_x e2 uty
  (* Let bound *)
  | Let (i, vi_x, e1, e2), _ ->
    inferUType e1 <->=
    (fun uty_x -> (vi_x  |:| uty_x) (checkUType e2 uty))


and check_update (i: info) (e1: expr) (e2 : expr)(e3 : expr) (p:predicate) (k_m: iterm)=
  fun (ctx, k) ->
    match (inferUType e1  ctx) with
    | Right (uty_1, c_1, psi_1, k_1) ->
       begin
         match uty_1  with
         | UArray (g, l, uty) ->
            if (List.mem_assoc g p) then
              begin
                match (inferUType e2  ctx) with
                | Right (uty_2, c_2, psi_2, k_2) ->
                 unary_debug dp "update_test star :@\n@[psi_2 %a, c_2 is %a @]@."  Print.pp_ivar_ctx psi_2 Print.pp_cs c_2 ;
                   begin
                     match uty_2 with
                     | UInt (l') -> if  (handle_u_update p g l' l ctx i psi_2 c_2) then
                                      begin
                                        let k_sum = option_combine k_1 k_2 (fun (ik,k') -> add_costs( add_costs (ik, ISucc (IZero) ), k') ) in
                                        let k_3 = option_combine (Some k_m) k_sum (fun(ik, k') -> IMinus(ik, k')  ) in
                                        unary_debug dp "update_test :@\n@[k is %a, k_sum is %a, k_3 is %a, e3 is %a @]@." Print.pp_cost k Print.pp_cost k_sum Print.pp_cost k_3 Print.pp_expr e3;
                                        match (checkUType e3 uty (ctx, k_3)) with
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
            (* match beta with
            | None -> false
            | Some (IArray(o, ls) ) -> 
                let c_1 =  CArrPos ( IArray(o,ls) , l' ) in 
                    let c_2 = merge_cs  (CLeq (l', l)) c_1 in 
                      let c_3 =  CImpl (ctx.constr_env, c_2) in 
                        let c_4 = quantify_all_exist ctx.evar_ctx c_3 in 
                          let c_5 = quantify_all_universal ctx.ivar_ctx c_4 in
                            let w_c= WhyTrans.why3_translate_int c_5 in 
                            unary_debug dp "handler_u_updte3 : @[  w_c: %a @]"  Why3.Pretty.print_term w_c;
                              WhySolver.post w_c 1
            | _ -> false *)




      (* let beta = (List.assoc_opt g p) in
        match beta, l',l with
         | None, _ ,_-> false
         | Some (IArray(a,ls)), IVar v, IVar v' -> 
         unary_debug dp "handler_u_updte2 : @[ v:%a, v': %a @]" Print.pp_vinfo v Print.pp_vinfo v';
            let c = CForall(v',i, Size,  CForall(v, i, Size, CImpl (ctx.constr_env, CLeq(l',l) ) ) ) in 
               let w_c=  WhyTrans.why3_translate c in 
               unary_debug dp "handler_u_updte3 : @[  w_c: %a @]"  Why3.Pretty.print_term w_c;
          let i_1'=  WhyTrans.get_why3_var v.v_name in
            let a_fml = WhySolver.createArrayTerm a.v_name in
              let get_a_i = WhySolver.createGetTerm a_fml i_1' in 
                let fml_btrue= WhySolver.checkValueTrue get_a_i in 
                let fml_1 = WhySolver.createImplyPredicate a_fml fml_btrue in 
                let cs_1= Why3.Term.t_forall_close [i_1'] [] fml_1 in
                let cs_2 =WhySolver.createExistArray a_fml cs_1 in
                    let    cs_3 =  Why3.Term.t_binary Why3.Term.Tand w_c cs_2 in
                 WhySolver.post cs_3 1 *)


  (* let beta = (List.assoc_opt g p) in
  unary_debug dp "in handle_update  :@\n@[g is %a, iterm l' is %a, l is %a, predicate is %a @]@."  Print.pp_vinfo g  Print.pp_iterm l'  Print.pp_iterm l Print.pp_predicates p;
     match (iterm_simpl l') , (iterm_simpl l), beta with
     | IConst i', IConst i , Some (IArray a) ->
        let x' = int_of_float i' in
        let x = int_of_float i in
         if  (x' < x ) && ( a.(x') = 1 ) then true else false
     | _, _, _ -> false *)
                    
and check_read (i: info) (e1: expr) (e2 : expr) (p:predicate) (k_m: iterm)  =
  fun (ctx,k) -> match (inferUType e1  ctx) with
                 | Right (uty_1, c_1, psi_1, k_1) ->
                    begin
                       unary_debug dp "read first premise :@\n@[type is %a, expr is %a, predicate is %a @]@."  Print.pp_type uty_1 Print.pp_expr e1 Print.pp_predicates p;
                         match uty_1 with
                         | UArray (g, l, uty) ->
                          
                            if (List.mem_assoc g p) then
                            begin
                              match (inferUType e2 ctx) with
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
   

and check_alloc (i: info) (e1 : expr) (e2 : expr) (k: iterm) (mu:mode) (g: var_info) (uty: ty) =
  match uty with
  |  UArray (g_1, l_1, uty_1) -> if g = g_1 then 
                                  check_body ( with_mode mu ( (checkUType e1 (UInt(l_1)) ) >> (checkUType e2 uty_1) ) ) ( minus_cost k (IConst 1.0) ) else fail i (WrongUShape (uty, "alloc return type"))
  | _ -> fail i (WrongUShape (uty, "alloc return type"))

and check_fix (i: info) (vi_f : var_info) (vi_x : var_info) (e : expr) (uty : ty) =
  match uty with
  | UTyArr(ty1, mu, k_fun, ty2) ->
  unary_debug dp "ck_fix:@\n@[e %a, ty1: %a, ty2: %a @]@.@\n"  Print.pp_expr e Print.pp_type ty1 Print.pp_type ty2;
    check_body ((vi_f |:| uty)
                  ((vi_x |:| ty1)
                     (with_mode mu (checkUType e ty2)))) k_fun
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
  inferUType e <->=
  (fun uty_g ->
     match uty_g with
     | UTyPrim UPrimBool -> (checkUType el uty) >&&> (checkUType er uty)
     | _ -> fail i (WrongUShape (uty, "bool")))

and check_nil i uty =
  match uty with
  | UTyList(n, ty) -> check_size_eq n IZero return_leaf_ch
  | _ -> fail i (WrongUShape (uty, "list"))


and check_cons e1 e2 i uty =
  match uty with
  | UTyList(n, ty) ->
    checkUType e1 ty >>
    (* Introduce a new size variable and add it to the existential ctx*)
    let v = fresh_evar Size in
    let sz = IVar v in
    (v |:::| Size) i
      (* Check that (sz + 1 = n) *)
      (check_size_eq (n) (ISucc sz)
         (checkUType e2 (UTyList(sz, ty))))
  | _ -> fail i (WrongUShape (uty, "list"))


and check_case_list i e e_n x_h x_tl e_c uty =
  inferUType e <->=
  (fun uty_g ->
     match uty_g with
     | UTyList (n, tye) ->
       (* Nil case *)
       (assume_size_eq n IZero (checkUType e_n uty))
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
               ( (x_tl |:| UTyList(sz, tye)) (checkUType e_c uty))))
     | _ -> fail i (WrongUShape (uty, "list"))
  )

and check_case_sum i e x_l e_l x_r e_r  uty =
  inferUType e <->=
  (fun uty_g ->
     match uty_g with
     | UTySum (tyl, tyr) -> 
       ((x_l |:| tyl) (checkUType e_l uty)) >>>=
       fun c1 -> (x_r |:| tyr) (checkUType e_r uty) >>>=
       fun c2 -> return_ch (merge_cs c1 c2) 
     | _ -> fail i (WrongUShape (uty, "sum"))
  )

and check_pack i e uty =
  match uty with
  | UTyExists(x, s, ty) ->
    let v = fresh_evar s in
    let witness = IVar v in
    (v |:::| s) i (checkUType e (un_ty_subst x witness ty))
  | _ -> fail i (WrongUShape (uty, "existential"))

and check_unpack i e1 vi_x e2 uty =
  inferUType e1 <->=
  (fun uty_x ->
     match uty_x with
     | UTyExists(x, s, ty) ->
       (x |::| s) i ((vi_x |:| ty) (checkUType e2 uty))
     | _ -> fail i (WrongUShape (uty, "existential")))

and check_clet i vi_x e1 e2 uty =
  inferUType e1 <->=
  (fun csty ->
     match csty with
     | UTyCs(cs, uty_x) ->
        (vi_x |:| uty_x) (checkUType e2 uty) >>= fun cs_b -> return_ch (CImpl(cs, cs_b))
     | _ -> fail i (WrongUShape (uty, "constrained")))




and infer_and_check (i: info) (e: expr) (uty : ty) : constr checker =
  fun(ctx, k) ->
    match inferUType e ctx with
    | Right (inf_uty, c, psi_ctx, k') ->
      unary_debug dp "infer_and_check :@\n@[infer_type is %a, expr is %a, checked type is %a, idx_ctx is :%a , k' is %a, k is %a @]@." 
      Print.pp_type inf_uty Print.pp_expr e Print.pp_type uty Print.pp_ivar_ctx ctx.ivar_ctx Print.pp_cost k' Print.pp_cost k; 
      (match (check_usubtype i inf_uty uty (extend_e_ctx psi_ctx ctx, None)) with
       | Right c' -> 
	  let cs = option_combine k' k (cost_cs ctx) |> Core.Option.value ~default:CTrue in
    unary_debug dp "infer_and_check2 :@\n@[cs is %a, c is %a, c' is %a @]@." 
      Print.pp_cs cs Print.pp_cs c Print.pp_cs c' ; 
          Right (quantify_all_exist psi_ctx (merge_cs (merge_cs c c') cs))
       | Left err -> Left err)
    | Left err' -> Left err'


let check_type ctx program ty  =
  match checkUType program ty ctx with
  | Right c ->  c
  | Left err -> typing_error_pp  err.i pp_tyerr err.v


 
