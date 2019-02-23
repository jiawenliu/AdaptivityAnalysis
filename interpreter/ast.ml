(* The type of the abstract syntax tree (AST). *)
type expr =
  | Var of string
  | App of expr*expr
  | Pair of expr*expr
  | True
  | False
  | Nil
  | Cons of expr*expr
  | Let string*expr*expr
  | Fst of expr
  | Snd of expr
  | If of expr*expr*expr
  | Const of int
  | Mech of expr
  |


 type value = 
 	| v_true of expr
 	|

