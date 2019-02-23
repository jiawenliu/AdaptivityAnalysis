open Ast

(* [subst e1 e2 x] is [e1] with [e2] substituted for [x]. *)
let rec subst e1 e2 x = 
  match e1 with
  

(* A single step of evaluation. *)
let rec bigstep env expr = 
  match expr with
  | Var x               -> failwith "Unbound variable"
  | Add(Int n1, Int n2) -> Int (n1+n2)
  | Add(Int n1, e2)     -> Add(Int n1, step e2)
  | Add(e1,e2)          -> Add(step e1, e2)
  | Let(x,Int n,e2)     -> subst e2 (Int n) x
  | Let(x,e1,e2)        -> Let(x,step e1, e2)


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
      

