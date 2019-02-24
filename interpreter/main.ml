open Ast

(* [subst e1 e2 x] is [e1] with [e2] substituted for [x]. *)
let rec subst e1 e2 x = 
  match e1 with
    | 
  

(* Big step of evaluation. *)
let rec bigstep env expr = 
  match expr with
  | Var x               -> (eval env x , T_Var x)
  | Const c             -> (V_Const c, T_Const c)
  | True                -> (V_True, T_True)
  | False               -> (V_False, T_False)
  | Fix f x e           -> ((V_Fix (Fix f x e) env), T_Fix (Fix f x e))
  | App e1 e2           -> 
    (
      match ((bigstep env e1), (bigstep env e2)) with
      | ((v1, t1), (v2, t2))    ->          
        (
          match v1 with
          | (Fix f x e, env1)   ->
            (
              match (bigstep ((f, v1) :: (x, v2) :: (env1)) e) with
              | (v, t)          -> (v, T_Eval t1 t2 (f, x) t)
              | _               -> Error
              )
          | _                   -> Error
          )
      | _                       -> Error
      )
  | Pair e1 e2          -> 
  | Fst e               ->
  | Snd e               ->
  | If True e1 e2       ->
  | If False e1 e2      ->
  | Mech e              ->
  | Nil                 -> (V_Nil, T_Nil)


(* fetch the value of variable from environments. *)
let rec eval env x =
  match env with
    | 

(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)
let parse s =
  

(* Extract a value from an ast node.
   Raises Failure if the argument is a node containing a value. *)
let extract_value = 

(* Interpret an expression *)
let interp e =

(* A few test cases *)
let run_tests () =
      

