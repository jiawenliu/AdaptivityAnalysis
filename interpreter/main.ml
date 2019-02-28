open Ast

(* The mechanism *)
let mech (v: value) : value = V_Const 12


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
      | ((v1, t1), (v2, t2))      ->          
        (
          match v1 with
          | (Fix f x e, env1)     ->
            (
              match (bigstep ((f, v1) :: (x, v2) :: (env1)) e) with
              | (v, t)            -> (v, T_Eval t1 t2 (f, x) t)
              | _                 -> Error
            )
          | _                     -> Error
        )
      | _                         -> Error
    )
  | Pair e1 e2          -> 
    (
      match ((bigstep env e1), (bigstep env e2)) with
        |((v1, t1), (v2, t2))     -> (V_Pair v1 v2, T_Pair t1 t2)
        |_                        -> Error
    )
  | Fst e               ->
    (
      match (bigstep env e) with
        | (V_Pair v1 v2, t)       -> (v1, T_Fst t)
        | _                       -> Error
    )
  | Snd e               ->
    (
      match (bigstep env e) with
        | (V_Pair v1 v2, t)       -> (v2, T_Snd t)
        | _                       -> Error
    )
  | If e e1 e2          ->
    (
      match ((bigstep env e),(bigstep env e1)) with
        | ((V_True, t), (v1, t1)) -> (v1, T_Iftrue t t1)
        | _                       -> Error
    )
  | If e e1 e2          ->
    (
      match ((bigstep env e),(bigstep env e1)) with
        | ((V_False, t), (v1, t1))-> (v1, T_Iffalse t t1)
        | _                       -> Error
    )
  | Mech e              -> 
    (
      match (bigstep env e) with
        | (v, t)                  -> 
        (
          match (mech v)  with
            | v1                  -> (v1, T_Mech t)
            | _                   -> Error
        )
        | _                       -> Error
    )
  | Nil                 -> (V_Nil, T_Nil)
  | Cons e1 e2          -> 
  (
    match ((bigstep env e1), (bigstep env e2)) with
      | ((v1, t1), (v2, t2))      -> (V_Cons v1 v2, T_Cons t1 t2)
      | _                         -> Error
  )
  | Let x e1 e2         -> 
  (
    match (bigstep env e1) with
      | (v1, t1)                  -> 
      (
        match (bigstep (x, v1)::env e2) with
          | (v, t2)               -> (v, T_Let x t1 t2)
          | _                     -> Error
      )
      | _                         -> Error
  )
  | _                   -> Error


(***********************************************************************)
(***********************************************************************)

(* fetch the value of variable from environments. *)
let rec eval env x =
  match env with
    | 
    | []    -> 


(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* Extract a value from an ast node.
   Raises Failure if the argument is a node containing a value. *)
let extract_value = function
  | V_True          -> "true"
  | V_False         -> "false"
  | V_Const i       -> string_of_int i
  | V_Fix(v, env)  -> 
  | V_Pair(v1, v2)  -> 
  | V_Nil           -> "[]"
  | V_Cons(v1, v2)  -> 
  | _ -> failwith "Not a value"

(* Interpret an expression *)
let interp e =
  e |> parse |> bigstep |> extract_value


(* A few test cases *)
let run_tests () =
      

