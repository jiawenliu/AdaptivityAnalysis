(* The type of the abstract syntax tree (AST). *)
type expr =
  | Var 	of string
  | App 	of expr * expr
  | Pair 	of expr * expr
  | True
  | False
  | Nil
  | Cons 	of expr*expr
  | Let 	of string * expr * expr
  | Fst 	of expr
  | Snd 	of expr
  | If 		of expr * expr * expr
  | Const 	of int
  | Mech 	of expr
  |


 type value = 
 	| V_True
 	| V_False
 	| V_Var		of string
 	| V_Const 	of int
 	| V_Nil


type trace = 
	| T_True
	| T_False
	| T_Var of string
	| T_Const of int
	| T_Nil