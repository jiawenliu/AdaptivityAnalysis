open BeAst
open Format

let inprog = ref (None : string option)
let infile = ref (None : string option)
let isfile = ref false


let argDefs = [
    "-ip", Arg.String (fun s -> inprog := Some s  ), "specify the input program string, -ip string" ; 
      "-if", Arg.String (fun s -> infile := Some s; isfile := true ), "specify the input file name, -if string" 
]

let parseArgs () =  
        Arg.parse argDefs 
        (fun s -> 
                match !inprog  with 
                      | Some (_) -> printf "%s" "specify just the programs"  
                      | None  -> inprog := Some (s) ) " " ;
             match !inprog  with
                   | Some i -> (i)
                   | _ -> 
                   (
                    match !infile with
                          | Some i -> (i)
                          | _ -> printf "%s" "specify  your input file -if or intput program string -ip"; ""
                    )


(* The mechanism *)
(*let mech (v: value) : value = V_Const 12
*)
(* fetch the value of variable from environments. *)
(*let rec eval (env: (expr * value) list) (x: expr): value =
  match env with
    | (var, v)::env   -> if (var = x) then v else eval env x
    | []              -> V_Error

*)
(* [subst e1 e2 x] is [e1] with [e2] substituted for [x]. *)
(*let rec subst e1 e2 x = 
  match e1 with
    | *)
  

(* Big step of evaluation. *)
(*let rec bigstep (env: (expr * value) list) (expr: expr): (value * trace) = 
  match expr with
  | Var x                 -> (eval env (Var x) , T_Var x )
  | Const c               -> (V_Const c, T_Const c)
  | True                  -> (V_True, T_True)
  | False                 -> (V_False, T_False)
  | Fix(f, x, e)          -> ((V_Fix ((Fix(f, x, e)), env)), T_Fix (Fix(f, x, e)))
  | App(e1, e2)           -> 
    (
      match ((bigstep env e1), (bigstep env e2)) with
      | ((v1, t1), (v2, t2))      ->          
        (
          match v1 with
          | V_Fix (Fix(f, x, e), env1)     ->
            (
              match (bigstep ((f, v1) :: (x, v2) :: (env1)) e) with
              | (v, t)            -> (v, T_Eval(t1, t2, (f, x), t))
              | _                 -> (V_Error, T_Error)
            )
          | _                     -> (V_Error, T_Error)
        )
      | _                         -> (V_Error, T_Error)
    )
  | Pair (e1, e2)          ->
    (
      match ((bigstep env e1), (bigstep env e2)) with
        |((v1, t1), (v2, t2))     -> (V_Pair(v1, v2), T_Pair (t1, t2))
        |_                        -> (V_Error, T_Error)
    )
  | Fst e                 ->
    (
      match (bigstep env e) with
        | (V_Pair (v1, v2), t)       -> (v1, T_Fst t)
        | _                       -> (V_Error, T_Error)
    )
  | Snd e                 ->
    (
      match (bigstep env e) with
        | (V_Pair (v1, v2), t)       -> (v2, T_Snd t)
        | _                       -> (V_Error, T_Error)
    )
  | If (e, e1, e2)        ->
    (
      match ((bigstep env e),(bigstep env e1)) with
        | ((V_True, t), (v1, t1)) -> (v1, T_Iftrue (t, t1))
        | ((V_False, t), (v1, t1))-> (v1, T_Iffalse (t, t1))
        | _                       -> (V_Error, T_Error)
    )
  | Mech e                -> 
    (
      match (bigstep env e) with
        | (v, t)                  -> 
        (
          match (mech v)  with
            | v1                  -> (v1, T_Mech t)
            | _                   -> (V_Error, T_Error)
        )
        | _                       -> (V_Error, T_Error)
    )
  | Nil                   -> (V_Nil, T_Nil)
  | Cons (e1, e2)         -> 
  (
    match ((bigstep env e1), (bigstep env e2)) with
      | ((v1, t1), (v2, t2))      -> (V_Cons(v1, v2), T_Cons(t1, t2) )
      | _                         -> (V_Error, T_Error)
  )
  | Let (x, e1, e2)       -> 
  (
    match (bigstep env e1) with
      | (v1, t1)                  -> 
      (
        match (bigstep ((x, v1)::env) e2) with
          | (v, t2)               -> (v, T_Let (x, t1, t2))
          | _                     -> (V_Error, T_Error)
      )
      | _                         -> (V_Error, T_Error)
  )
  | _                     -> (V_Error, T_Error)

*)
(***********************************************************************)
(***********************************************************************)


(* Parse a string into an ast *)

let parse_string prog =
  if (!isfile) 
  then
    let fh = open_in prog 
    in
      let lb = (Lexing.from_channel fh) in 
        BeParser.expr BeLexer.token lb
  else 
    let lb = Lexing.from_string prog
    in
      BeParser.expr BeLexer.token lb

(* Extract a value from an ast node.
   Raises Failure if the argument is a node containing a value. *)
(*let extract_value = function
  | V_True          -> "true"
  | V_False         -> "false"
  | V_Const i       -> string_of_int i
  | V_Fix(v, env)   -> "Fix Function Value"
  | V_Pair(v1, v2)  -> "(Pair value)"
  | V_Nil           -> "[]"
  | V_Cons(v1, v2)  -> "List Value"
  | _               -> failwith "Not a value"
*)
(* Interpret an expression *)
(*let interp e =
  e |> parse |> bigstep |> extract_value
*)

(* Print all tokens *) 
let token_list_of_string s =
  let lb = Lexing.from_string s in
  let rec helper l =
    try
      let t = BeLexer.token lb in
      if t = BeParser.EOF then List.rev l else helper (t::l)
    with _ -> List.rev l
  in 
    helper []

let rec pretty_print (e : BeAst.expr) = 
  match e with
  | Var s           -> Printf.printf " Var %s " s
  | Const i         -> Printf.printf " Const %d " i
  | True            -> Printf.printf " True " 
  | False           -> Printf.printf " False "
  | Pair(e1, e2)    -> Printf.printf " ("; pretty_print e1 ; Printf.printf ","; pretty_print e2; Printf.printf ") "
  | App (e1, e2)    -> pretty_print e1 ; Printf.printf " "; pretty_print e2
  | Fix(e1, e2, e3) -> Printf.printf " Fix "; pretty_print(e1); Printf.printf " ("; pretty_print (e2); Printf.printf ") "; pretty_print(e3)
  | Fst e           -> Printf.printf " Fst "; pretty_print(e)
  | Snd e           -> Printf.printf " Snd "; pretty_print(e)
  | If(e, e1, e2)   -> Printf.printf " If("; pretty_print(e); Printf.printf " then "; pretty_print(e1); Printf.printf "else"; pretty_print(e2)
  | Mech e          -> Printf.printf " Mech("; pretty_print(e); Printf.printf ") "
  | Let(x, e1, e2)  -> Printf.printf " Let "; pretty_print(x); Printf.printf " = "; pretty_print(e1); Printf.printf " in "; pretty_print(e2)
  | Nil             -> Printf.printf " [] "
  | Cons(e1, e2)    -> Printf.printf " Cons("; pretty_print(e1); Printf.printf ", "; pretty_print(e2); Printf.printf ") "

(*let e = (parse_string "let x = 12 in (x1, x2)" in (pretty_print e)*)
let main = 
  let prog = parseArgs () in pretty_print (parse_string prog); print_endline ""


