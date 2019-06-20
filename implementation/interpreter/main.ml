open Syntax
open IndexSyntax
open Format
open Print

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

let main = 
  let prog = parseArgs () in 
    match (parse_string prog) with 
    | (expr, ty) -> pp_expression std_formatter expr ; pp_type std_formatter ty


