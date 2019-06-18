open Syntax
open IndexSyntax
open Format

let inprog = ref (None : string option)
let infile = ref (None : string option)
let isfile = ref false



let argDefs = [
    "-prog", Arg.String (fun s -> inprog := Some s  ), "specify the input program string, -ip string" ; 
      "-file", Arg.String (fun s -> infile := Some s; isfile := true ), "specify the input file name, -if string" 
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



(* Parsing string *)

let parse_string prog =
  if (!isfile) 
  then
    let fh = open_in prog 
    in
      let lb = (Lexing.from_channel fh) in 
        Parser.toplevel Lexer.main lb
  else 
    let lb = Lexing.from_string prog
    in
      Parser.toplevel Lexer.main lb

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
      if t = Parser.EOF  then List.rev l else helper (t::l)
    with _ -> List.rev l
  in 
    helper []

let pp_bop fmt (p : Syntax.bop) = 
  match p with
    | Add           -> fprintf fmt " + "
    | Sub         -> fprintf fmt " - "
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
    | Setminus      -> fprintf fmt " \ "

let pp_uop fmt (p : Syntax.uop) = 
  match p with
    | Sign          -> fprintf fmt "sign"
    | Log            -> fprintf fmt "log"

let rec pp_expression fmt (e : Syntax.expr) = 
  match e with
  | Var v             -> fprintf fmt " Var %s " v.v_name
  | Prim p            -> (match p with 
    | PrimInt i       -> fprintf fmt " Int %d " i
    | PrimUnit        -> fprintf fmt " Unit "
    | PrimReal r      -> fprintf fmt " Real %f " r
    )              
  | True              -> fprintf fmt " True " 
  | False             -> fprintf fmt " False "
  | Pair(e1, e2)      -> fprintf fmt " (%a, %a)"  pp_expression e1 pp_expression e2
  | App (e1, e2)      -> fprintf fmt " App @[%a@] @[%a@] " pp_expression e1  pp_expression e2
  | Fix(f, x, e3)     -> fprintf fmt " Fix %s (%a@). @\n@[<hov 1> %a@]@\n" f.v_name pp_expression(x) pp_expression(e3)
  | Fst e             -> fprintf fmt " Fst %a " pp_expression(e)
  | Snd e             -> fprintf fmt " Snd %a " pp_expression(e)
  | If(e, e1, e2)     -> fprintf fmt " If %a Then @\n @[<hov 1> %a@]@\n Else @\n @[<hov 1> %a@]@\n" pp_expression(e)  pp_expression(e1) pp_expression(e2)
  | Mech e            -> fprintf fmt " Mech( %a )" pp_expression(e)
  | Let(x, e1, e2)    -> fprintf fmt " @[<v>@[<hov> Let %s =@;<1 1>@[%a@]@] in@ %a@]" x.v_name pp_expression(e1) pp_expression(e2)
  | Nil               -> fprintf fmt " [] "
  | Cons(e1, e2)      -> fprintf fmt " %a :: %a " pp_expression(e1) pp_expression(e2)
  | Bop(p, e1, e2)    -> fprintf fmt " @[%a@] %a @[%a@] " pp_expression(e1) pp_bop(p) pp_expression(e2)
  | Uop(p, e)         -> fprintf fmt " %a ( %a ) " pp_uop(p)  pp_expression(e)
  | IApp e            -> fprintf fmt " %a []" pp_expression(e)
  | ILam e            -> fprintf fmt " Lam. %a " pp_expression(e)
  | _                 -> fprintf fmt " new "

(*let e = (parse_string "let x = 12 in (x1, x2)" in (pp_expression e)*)
let main = 
  let prog = parseArgs () in 
    match (parse_string prog) with 
    | (expr, ty) -> pp_expression std_formatter expr


