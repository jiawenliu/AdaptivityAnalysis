(* The type of the abstract syntax tree (AST). *)
type expr =
  | Var 	  of string
  | Const   of int
  | True
  | False
  | Pair    of expr * expr
  | App     of expr * expr
  | Fix     of expr * expr * expr(* unsure *) 
  | Fst     of expr
  | Snd     of expr
  | If      of expr * expr * expr
  | Mech 	  of expr
  | Let     of expr * expr * expr
  | Nil
  | Cons 	  of expr*expr


 type value = 
 	| V_True
 	| V_False
  | V_Const 	of int
  | V_Fix		of expr * ((expr * value) list) (* unsure *) 
  | V_Pair 	of value * value
 	| V_Nil
 	| V_Cons 	of value * value
  | V_Error


type trace = 
	| T_Var		of string
	| T_Eval	of trace * trace * (expr * expr) * trace (* unsure *) 
	| T_Fix 	of expr
	| T_Pair	of trace * trace
	| T_Fst		of trace
	| T_Snd		of trace
	| T_True
	| T_False
	| T_Iftrue	of trace * trace
	| T_Iffalse	of trace * trace
	| T_Const 	of int
	| T_Mech 	of trace
	| T_Nil
	| T_Cons 	of trace * trace
	| T_Let 	of expr * trace * trace
  | T_Error

type error = Error
