open Syntax
open Format
open Support.FileInfo
open Support.Error

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
        Parser.u_toplevel Lexer.main lb
  else 
    let lb = Lexing.from_string prog
    in
      Parser.u_toplevel Lexer.main lb

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
      let t = Lexer.main lb in
      if t = Parser.EOF then List.rev l else helper (t::l)
    with _ -> List.rev l
  in 
    helper []

let pp_bop fmt (p : Syntax.bop) = 
  match p with
    | Plus          -> fprintf fmt " + "
    | Minus         -> fprintf fmt " - "
    | Mul           -> fprintf fmt " * "
    | Div           -> fprintf fmt " / " 
    | Or            -> fprintf fmt " || "
    | And           -> fprintf fmt " && "
    | Xor           -> fprintf fmt " ^ " 
    | Equal         -> fprintf fmt " = "
    | Leq           -> fprintf fmt " <= " 
    | Geq           -> fprintf fmt " >= " 
    | Less          -> fprintf fmt " < " 
    | Greater       -> fprintf fmt " > "

let pp_uop fmt (p : Syntax.uop) = 
  match p with
    | Sign          -> fprintf fmt "sign"
    | Log            -> fprintf fmt "log"

let rec pp_expression fmt (e : Syntax.expr) = 
  match e with
  | Var s             -> fprintf fmt " Var %s " s
  | Const_i i         -> fprintf fmt " Int %d " i
  | Const_f f         -> fprintf fmt " Float %f " f
  | True              -> fprintf fmt " True " 
  | False             -> fprintf fmt " False "
  | Pair(e1, e2)      -> fprintf fmt " (%a, %a)"  pp_expression e1 pp_expression e2
  | App (e1, e2)      -> fprintf fmt " App @[%a@] @[%a@] " pp_expression e1  pp_expression e2
  | Fix(e1, e2, e3)   -> fprintf fmt " Fix %a (%a). @\n@[<hov 1> %a@]@\n" pp_expression(e1) pp_expression (e2) pp_expression(e3)
  | Fst e             -> fprintf fmt " Fst %a " pp_expression(e)
  | Snd e             -> fprintf fmt " Snd %a " pp_expression(e)
  | If(e, e1, e2)     -> fprintf fmt " If %a Then @\n @[<hov 1> %a@]@\n Else @\n @[<hov 1> %a@]@\n" pp_expression(e)  pp_expression(e1) pp_expression(e2)
  | Mech e            -> fprintf fmt " Mech( %a )" pp_expression(e)
  | Let(x, e1, e2)    -> fprintf fmt " @[<v>@[<hov> Let %a =@;<1 1>@[%a@]@] in@ %a@]" pp_expression(x) pp_expression(e1) pp_expression(e2)
  | Nil               -> fprintf fmt " [] "
  | Cons(e1, e2)      -> fprintf fmt " %a :: %a " pp_expression(e1) pp_expression(e2)
  | Bop(p, e1, e2)    -> fprintf fmt " @[%a@] %a @[%a@] " pp_expression(e1) pp_bop(p) pp_expression(e2)
  | Uop(p, e)         -> fprintf fmt " %a ( %a ) " pp_uop(p)  pp_expression(e)
  | _                 -> fprintf fmt " new "

(*let e = (parse_string "let x = 12 in (x1, x2)" in (pp_expression e)*)
let main = 
  let prog = parseArgs () in pp_expression std_formatter (parse_string prog)


