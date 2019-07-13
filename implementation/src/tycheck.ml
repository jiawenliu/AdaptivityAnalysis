(* ---------------------------------------------------------------------- *)
(* Core Typechecking Engine for Adapt                               *)
(* ---------------------------------------------------------------------- *)

open Tycheck_sigs
open Map


module TyCheck (Ty : CH_TYPE) =
  struct
    open IndexSyntax
    open Fresh_var
    open Constr
    open Ctx
    open Syntax
    open Support.FileInfo
    open Core
    open DMap

    module Map = Map.Make(var_info)
    module Opt = Support.Options

    let dp = Support.FileInfo.dummyinfo
    let tc_debug   fi = Support.Error.message 4 General fi

    let typing_err fi = Support.Error.error_msg  Opt.TypeChecker fi

    type 'a ty_error = 
          Right of 'a
        | Left  of Ty_error.ty_error_elem withinfo
    type ty = Ty.ty
   
   (* The Adaptivity of programs *)
    type adapt = iterm


   (* The Depth Map of variables in programs *)
    type dmap = 'a Map.t
 
   (* The context for Type Checking *)
    type ch_ctx = ty context * adapt

   (* The context for Type Inference *) 
    type inf_ctx = ty context

   (* Reader/Error monad for  type-checking *)
    type 'a checker = ch_ctx -> ('a * dmap * adapt) ty_error

    let return_ch cs  = fun ch_ctx -> Right cs

    (* Reader/Error monad for type-inference *)
    type 'a inferer =  inf_ctx -> ('a * constr * dmap * adapt) ty_error

    let adapt_leq_cs ctx (z1, z2) =
       CLeq(iterm_simpl z2, iterm_simpl z1)



    let if_adapt k' = Option.map ~f:(fun _ -> k')

    let extend_adapt v1 ctx = extend_e_var v1.v_name Adapt ctx 

    let (>>)  (m : constr checker) (m' : constr checker) : constr checker =
      fun (ctx, k) ->
      (* Generate two new adapt meta variables *)
      let v1 = fresh_evar in
      let v2 = fresh_evar in
      let k1 = (IVar v1) in
      let k2 = (IVar v2) in
      (* Extend the existential ctx with the two generated vars *)
      let k1_ctx = extend_adapt v1 ctx in
      let k2_ctx = extend_adapt v2 k1_ctx in
      let k_cs   = adapt_leq_cs ctx (add_adapts(k1, k2), k) in

      (* Call the checker on the first premise with adapt k1 *)
      begin
        match m (k1_ctx, if_adapt k1 k) with
        | Right c1 ->
           begin
             (* Call the checker on the second premise with adapt k2 *)
             match (m' (k2_ctx, if_adapt k2 k)) with
             | Right c2 ->
                (* Combine the constraints of two checkers with the adapt constraint k1+k2 <= k*)
                let base =  merge_cs c1 (merge_cs c2 k_cs) in
    
                (* Existentially quantify over the cosntraint with the new adapts k1 and k2 *)
                let c_quant = CExists(v1, UNKNOWN, Adapt, CExists(v2, UNKNOWN, Adapt, base)) in 
                 	let cs_res = Option.value_map ~default:base
                  		      ~f:(fun _ -> c_quant) k in
                        Right cs_res
             | Left err' -> Left err'
           end
        | Left err  -> Left err
      end


let (>>=) (m : 'a checker) (f : 'a -> 'b checker) : 'b checker =
  fun ch_ctx ->
    match m ch_ctx with
    | Right res -> f res ch_ctx
    | Left e    -> Left e


let (>>>=)  (m : 'a checker) (f : 'a -> 'b checker) : 'b checker =
    fun (ctx, k) ->
      match m (ctx, k) with
      | Right c -> f c (ctx,k)
      | Left err  -> Left err




(* Monadic combination with conjunction of contraints; ignores function *)
let (>&&>) m1 m2 = 
	m1 >>= (fun c1 -> m2 >>= fun c2 -> return_ch (merge_cs c1 c2))


let handle_fail m1 m2 =
	fun ch_ctx ->
	match m1 ch_ctx with
	  | Right c -> Right c
	  | Left _ -> m2 ch_ctx

(* Monadic combination with disjunction of constraints ; ignores function *)
let (>||>) m1 m2 =
	(handle_fail m1 m2) >>=  
		 (fun c1 -> handle_fail m2 (return_ch c1) >>=
			      fun c2 -> return_ch (COr(c1,c2)))


(* Type inference *)
let (<<=) (m : 'a inferer) (f : 'a -> 'b inferer) : 'b inferer =
        fun ctx ->
          match m ctx with
          | Right (res, c, psi, k) ->
             begin
               match (f res ctx) with
               | Right (res',c', psi', k') -> 
                  Right (res', merge_cs c c', merge_dmaps psi psi', (add_adapts k k'))
               | Left err' -> Left err'
             end
          | Left err  -> Left err


(* Instead of generating an fresh adapt variable, we simply subtract
   the inference adapt from the checking adapt. Otherwise, merge-min breaks
   due to complicated existential substitution *)
let (<->=) (m : ty  inferer) (f : ty  -> constr checker) : constr checker =
    fun (ctx, k) ->
          match (m ctx) with
          | Right (ty, c, psi, k') ->
          tc_debug dp "<->= :@\n@[  c is %a, k is %a , k' is %a @]@."  Print.pp_cs c Print.pp_adapt k Print.pp_adapt k';  
            begin
              match (f ty (ctx, option_combine k k' (fun (ik,k') -> IMinus(ik, k')))) with
              | Right c' -> Right (quantify_all_exist psi (merge_cs c c'))
              | Left err' -> Left err'
            end          
          | Left err -> Left err


let (=<->) (m : ty inferer) (f: ty -> (constr checker * ty * iterm * dmap * iterm) ) : ty inferer =
    fun ctx ->
      match m ctx with
      | Right (ty, c1, dps1, z1) ->
        begin
          let (m', ty_inf, q, dps, z) = f ty in
          match m' (k_ctx ,if_adapt k' k) with
          | Right(c2, dps2, z2) -> 
            (* Calculate the new Depth Map *)
            let dps' = max_dmap dps1 (sum_adap_dmap z1 (max_dmap dps (sum_cons_dmap q dps2))) in
              (* Calculate the new Adaptivity *)
              let z' = add_adapts z1 (max_adapts z (sum_adap_depth z2 q) ) in
              Right (ty_inf, merge_cs c1 c2, dps', z')
          | Left err' -> Left err'
        end
      | Left err -> Left err





let return_inf(x : 'a) : 'a inferer = 
	fun ctx -> Right (x, empty_constr, empty_dmap, Some (IConst 0) )


let return_leaf_ch  = fun (ctx, k) -> 
    Right adapt_leq_cs ctx (IConst 0,k)       


let fail (e : Ty_error.ty_error_elem) = fun _ ->
    Left { i = UNKNOWN; v = e }


let get_infer_ctx : ty context inferer =
        fun ctx -> Right (ctx, empty_constr, [], None)


(*let get_heur  : heurMode checker =
        fun (ctx,_) -> Right ctx.heur_mode
*)
  

let get_var_ty (vi : var_info) : ty inferer =
        get_infer_ctx <<= fun ctx ->
          return_inf @@
            match (lookup_var vi.v_name ctx) with
              None ->  typing_err UNKNOWN "Identifier %s is unbound" vi.v_name
            | Some (v, ty) ->  ty


let with_new_ctx (f : ty context -> ty context) (m : 'a checker) : 'a checker =
  fun (ctx,k) -> m (f ctx, k)


let with_mode (mu :mode) (m : 'a checker) : 'a checker =
  with_new_ctx (set_exec_mode mu) m


let (|:|) (vi: var_info) (uty: ty) (m: constr checker) : constr checker =
  with_new_ctx (extend_var vi.v_name uty) m

let (|:-|) (vi: var_info) (uty: ty) (m: constr checker) : constr checker =
  with_new_ctx (extend_uvar vi.v_name uty) m

let (|:::|) (v: var_info) (s: sort) (i: info) (m: constr checker) : constr checker =
  with_new_ctx (extend_e_var v.v_name s) m
  >>= (fun cs -> return_ch @@ CExists(v, i, s, cs (* CAnd (CLeq(IConst 0, IVar v), cs) *)))

let (|::::|) (v: var_info) (s: sort) (i: info)(m: constr checker) : constr checker =
  with_new_ctx (extend_l_var v.v_name ) m


let (|::|) (v: var_info) (s: sort) (i: info)(m: constr checker) : constr checker =
    with_new_ctx (extend_i_var v.v_name s) m
	>>=
	  (fun cs ->
	   let r_cs = 
	     match s with 
	     | Size -> cs (* CImpl(CAnd(CLeq(IConst 0, IVar v), CEq(IFloor(IVar v), ICeil (IVar v))),cs) *)
	     | _ -> cs 
	   in return_ch @@ CForall(v, i, s, r_cs))

	  

let check_size_eq  (sl : iterm) (sr : iterm) (m: constr checker)  : constr checker =
	m >>= fun c -> return_ch @@ merge_cs c (CEq (sl,sr))


let assume_size_eq  (sl : iterm) (sr : iterm) (m: constr checker)  : constr checker =
	m >>= fun c -> return_ch @@ CImpl (CEq (sl,sr), c)


let assume_size_leq  (sl : iterm) (sr : iterm) (m: constr checker)  : constr checker =
        m >>= fun c -> return_ch @@ CImpl (CLeq (sl,sr), c)


let check_size_leq  (sl : iterm) (sr : iterm) (m: constr checker)  : constr checker =
        m >>= fun c -> return_ch @@ merge_cs c (CLeq (sl,sr))


let check_dmap_leq (sl) () () : constr checker =
      () 




  

end


