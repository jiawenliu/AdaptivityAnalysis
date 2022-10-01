
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

    type dmap = DMap.dmap


   (* The context for Type Checking *)
    type ch_ctx = ty context

   (* The context for Type Inference *) 
    type inf_ctx = ty context

    (* Reader/Error monad for Equivalent-checking *)
    type 'a equiv_checker = ch_ctx -> 'a ty_error

    (* Reader/Error monad for  type-checking *)
    type 'a checker = ch_ctx -> ('a * dmap * adapt) ty_error

    (* Reader/Error monad for type-inference *)
    type 'a inferer =  inf_ctx -> ('a * constr * dmap * adapt) ty_error


let return_eq_ch (cs : 'a) : 'a equiv_checker  = 
    fun ch_ctx -> Right cs

let return_leaf_eq_ch  = 
    fun ch_ctx -> Right empty_constr


let return_ch (cs : 'a) : 'a checker  = 
    fun ch_ctx -> Right (cs, empty_dmap, IConst 0)


let return_inf(x : 'a) : 'a inferer = 
    fun inf_ctx -> Right (x, empty_constr, empty_dmap, (IConst 0) )


let return_leaf_ch  = 
    fun ch_ctx -> 
        Right (empty_constr, empty_dmap, IConst 0)      



let (<<) (m1 : constr equiv_checker) (m2 : constr equiv_checker) : constr equiv_checker =
  fun ctx ->
      begin
        match m1 ctx with
        | Right c1 ->
           begin
             (* Call the checker on the second premise *)
             match (m2 ctx) with
             | Right c2 ->
                (* Combine the constraints of two checkers with the adapt constraint k1+k2 <= k*)
                let cs' =  merge_cs c1 c2 in
                      Right cs'
             | Left err' -> Left err'
           end
        | Left err  -> Left err
      end

let (<=<) (m : constr equiv_checker) (cs : constr) : constr equiv_checker =
  fun ctx ->
        match m ctx with
        | Right c -> Right (merge_cs c cs)
        | Left err -> Left err



let (>>)  (m : constr checker) (m' : constr checker) : constr checker =
      fun (ctx) ->
      (* Call the checker on the first premise *)
      begin
        match m ctx with
        | Right (c1, dps1, z1) ->
           begin
             (* Call the checker on the second premise *)
             match (m' ctx) with
             | Right (c2, dps2, z2) ->
                (* Combine the constraints of two checkers with the adapt constraint k1+k2 <= k*)
                let cs' =  merge_cs c1 c2 in
                  (*Generate the New Depth Map, is the Maximum of Existing Depth Maps*)
                  let dps' = max_dmaps dps1 dps2 in
                    (*Generate the New Adaptivity, is the Maximum of Existing Depth Adaptivity*)
                    let z' = max_adapts z1 z2 in   
                      Right (cs', dps', z')
             | Left err' -> Left err'
           end
        | Left err  -> Left err
      end


let (>>=) (m : constr checker) (f : constr -> constr checker) : constr checker =
  fun ctx ->
    match m ctx with
    | Right (cs, dps, z) ->
      begin 
        match (f cs ctx) with
          | Right(cs', dps', z') 
            -> Right(merge_cs cs cs', merge_dmaps dps dps', add_adapts z z')
          | Left err -> Left err
      end
    | Left e    -> Left e





(* Monadic combination with conjunction of contraints; with the Adaptivity added to all *)
let (>&&>) m1 m2 = 
  fun ctx ->
        match m1 ctx with
        | Right (c1, dps1, z1) ->
           begin
             (* Call the checker on the second premise *)
             match (m2 ctx) with
             | Right (c2, dps2, z2) ->
                (* Combine the constraints of two checkers with the adapt constraint k1+k2 <= k*)
                let cs' =  merge_cs c1 c2 in
                  (*Generate the New Depth Map, is the Maximum of Existing Depth Maps*)
                  let dps' = max_dmaps dps1 (sum_adap_dmap z1 dps2) in
                    (*Generate the New Adaptivity, is the Maximum of Existing Depth Adaptivity*)
                    let z' = add_adapts z1 z2 in   
                      Right (cs', dps', z')
             | Left err' -> Left err'
           end
        | Left err  -> Left err



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
               | Right (res', c', psi', k') -> 
                  Right (res', merge_cs c c', merge_dmaps psi psi', (add_adapts k k'))
               | Left err' -> Left err'
             end
          | Left err  -> Left err


(* Instead of generating an fresh adapt variable, we simply subtract
   the inference adapt from the checking adapt. Otherwise, merge-min breaks
   due to complicated existential substitution *)
let (<->=) (m : ty  inferer) (f : ty  -> (constr checker * dterm * var_info)) : constr checker =
    fun ctx ->
          match (m ctx) with
          | Right (ty1, c1, dps1, z1) ->
(*            tc_debug dp "<->= :@\n@[  c is %a, k is %a , k' is %a @]@."  
            Print.pp_cs c Print.pp_adapt k Print.pp_adapt k';  
*)           
            let (m, q, vi_x) = f ty1 in 
            begin
              match m ctx with
              | Right (c2, dps2, z2) ->
                let z = max_adapts z2 (add_adap_depth z1 q) in
                  let dps = max_dmaps dps2 (sum_depth_dmap q dps1) in
                    let dps = dmap_rm dps vi_x.v_name 
                    in 
                    let dpsx = get_depth dps2 vi_x.v_name in
                    begin
                    match dpsx with
                      | Some dpsx ->                   
                              let csd = depth_cs dpsx q in
                                
                                Right (CAnd(CAnd(c1, c2), csd), dps, z )
                      | None -> Left { i = UNKNOWN; v = Internal "Depth not Found"}

                    end

              | Left err' -> Left err'
            end          
          | Left err -> Left err


let (=<->) (m : ty inferer) (f: ty -> (constr checker * ty * dterm * dmap * iterm) ) : ty inferer =
    fun ctx ->
      match m ctx with
      | Right (ty, c1, dps1, z1) ->
        begin
          let (m', ty_inf, q, dps, z) = f ty in
          match m' ctx with
          | Right(c2, dps2, z2) -> 
            (* Calculate the new Depth Map *)
            let dps' = max_dmaps dps1 (sum_adap_dmap z1 (max_dmaps dps (sum_depth_dmap q dps2))) in
              (* Calculate the new Adaptivity *)
              let z' = add_adapts z1 (max_adapts z (add_adap_depth z2 q) ) in
              Right (ty_inf, merge_cs c1 c2, dps', z')
          | Left err' -> Left err'
        end
      | Left err -> Left err







let fail (e : Ty_error.ty_error_elem) = fun _ ->
    Left { i = UNKNOWN; v = e }


let get_infer_ctx : ty context inferer =
        fun ctx -> Right (ctx, empty_constr, empty_dmap, IConst 0)


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
  fun (ctx) -> m (f ctx)


let (|:|) (vi: var_info) (vty: ty) (m: constr checker) : constr checker =
  with_new_ctx (extend_var vi.v_name vty) m


(*let (|:::|) (v: var_info) (s: sort) (i: info) (m: constr checker) : constr checker =
  with_new_ctx (extend_e_var v.v_name s) m
  >>= (fun cs -> return_ch @@ CExists(v, i, s, cs ))
*)


let (|::|) (v: var_info) (s: sort) (m: constr checker) : constr checker =
    with_new_ctx (extend_i_var v.v_name s) m
	>>=
	  (fun cs -> return_ch @@ CForall(v, s, cs))

	  

let check_size_eq  (sl : iterm) (sr : iterm) (m: constr checker)  : constr checker =
	m >>= fun c -> return_ch @@ merge_cs c (CEq (sl,sr))


(*let assume_size_eq  (sl : iterm) (sr : iterm) (m: constr checker)  : constr checker =
	m >>= fun c -> return_ch @@ CImpl (CEq (sl,sr), c)


let assume_size_leq  (sl : iterm) (sr : iterm) (m: constr checker)  : constr checker =
        m >>= fun c -> return_ch @@ CImpl (CLeq (sl,sr), c)
*)

let check_size_leq  (sl : iterm) (sr : iterm) (m: constr checker)  : constr checker =
        m >>= fun c -> return_ch @@ merge_cs c (CLeq (sl,sr))



end


