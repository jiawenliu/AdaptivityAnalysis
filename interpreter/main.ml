open Ast

(* [subst e1 e2 x] is [e1] with [e2] substituted for [x]. *)
let rec subst e1 e2 x = 
  match e1 with
    | 
  

(* Big step of evaluation. *)
let rec bigstep env expr = 
  match expr with
  | Var x               -> (V_Var (eval env x) , T_Var x)
  | Const c             -> (V_Const c, T_Const c)
  | True                -> (V_True, T_True)
  | False               -> (V_False, T_False)
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
      

