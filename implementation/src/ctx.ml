open Syntax
open IndexSyntax
open Constr

(* Context management *)
(* ---------------------------------------------------------------------- *)
(* Contexts of type 'a *)
type 'a ctx = (var_info * 'a) list

(* Context *)
type 'a context =
    {
      var_ctx   : 'a ctx;
      ivar_ctx  : sort ctx;
      evar_ctx  : sort ctx;
      dmap_ctx  : iterm ctx;
(*    lvar_ctx  : var_info list;
*)    constr_env :  constr
    }

let length ctx = List.length ctx

let empty_context = { var_ctx = []; ivar_ctx = []; evar_ctx = []; (*lvar_ctx = [];*) constr_env = CTrue }

(* Return a binding if it exists. Let the caller handle the error *)
let rec slookup id ctx =
  match ctx with
      []                -> None
    | (var, value) :: l ->
      if var.v_name = id then
        Some (var, value)
      else
        slookup id l

let lookup_var id ctx =
  slookup id ctx.var_ctx

let lookup_ivar id ctx =
  slookup id ctx.ivar_ctx

(*let rec lookup_lvar id ctx =
    match ctx with
        [] -> None
      | v::l ->
        if(v.v_name = id) then 
          Some v
        else
          lookup_lvar id l
*)

let lookup_evar id ctx =
  slookup id ctx.evar_ctx

(* Extend the context with a new variable binding. *)
let extend_var id s ctx =
  let n_var = {
    v_name  = id
    } in
  {ctx with var_ctx   = (n_var, s) :: ctx.var_ctx }

(*for location environment *)
(*let extend_l_var id ctx =
  let n_var =
    {
    v_name= id
    } in
   {ctx with lvar_ctx = (n_var) :: ctx.lvar_ctx }
*)
(* Extend the index context with a new binding. Return the new context. *)
let extend_i_var id s ctx =
  let n_var = {
    v_name  = id
  } in
  { ctx with ivar_ctx = (n_var, s) :: ctx.ivar_ctx }


(* Extend the existential context with a new binding. Return the new context. *)
let extend_e_var id s ctx =
  let n_var = {
    v_name  = id
  } in
  { ctx with evar_ctx = (n_var, s) :: ctx.evar_ctx }

(* Extend the existential context with a list of bindings. Return the new context. *)
let extend_e_ctx psi ctx =
  List.fold_left (fun  ctx' (vi,s) -> extend_e_var vi.v_name s ctx') ctx psi


let extend_d_ctx id depth ctx =
  let n_var =
    {
      v_name = id
    } 
  in
    {
      ctx with dmap_ctx = (n_var, depth) :: ctx.dmap_ctx
    }

let set_context vctx context =
  {
    var_ctx  = vctx;
    ivar_ctx = context.ivar_ctx;
   (* lvar_ctx = context.lvar_ctx;*)
    evar_ctx = context.evar_ctx;
    dmap_ctx = context.dmap_ctx;
    constr_env = context.constr_env;
  }


