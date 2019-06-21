open Syntax
open IndexSyntax
open Format
open Print2Ocaml

let infile = ref (None : string option)
let outfile = ref (None : string option)



let argDefs = [
    "-i", Arg.String (fun s -> infile := Some s  ), "specify the input file name" ; 
    "-o", Arg.String (fun s -> outfile := Some s ), "specify the output file name" 
]

let parseArgs () =  
        Arg.parse argDefs 
        (fun s -> 
                match !infile  with 
                      | Some (_) -> printf "%s" "specify "  
                      | None  -> infile := Some (s) ) " " ;
             match !infile, !outfile  with
                   | Some inf, Some outf -> (inf, outf)
                   | Some inf, None  -> printf "specify your output file name by -o outfilename"; ("", "")
                   | None, Some outf -> printf "specify your input file name by -i infilename"; ("", "")
                   | _ -> printf "specify your input file name and output file name "; ("", "")



(* Parsing string *)

let parse_string file =
    let ic = open_in file 
    in
      let lb = (Lexing.from_channel ic) in 
        Parser.toplevel Lexer.main lb

let main = 
  let (infile, outfile) = parseArgs () in 
    let (expr, ty) = (parse_string infile) in
    let  out_formatter = formatter_of_out_channel (open_out ("../exec/"^outfile)) in
      pp_progm out_formatter expr ; 
      pp_type std_formatter ty

