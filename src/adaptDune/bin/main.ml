open Core
open Syntax
open Format
open Abs 
let infile = ref (None : string option)
let outfile = ref (None : string option)



let argDefs = [
    "-i", Arg.String (fun s -> infile := Some s  ), "specify the input file name" ;
    "-o", Arg.String (fun s -> outfile := Some s ), "specify the output file name" 
]

let parse_prog file =
    let ic = In_channel.create file
    in
      let lb = (Lexing.from_channel ic) in 
        Parser.toplevel Lexer.main lb

        let parseArgs () =  
          Arg.parse argDefs 
          (fun s -> 
                  match !infile  with 
                        | Some (_) -> printf "%s" "specify "  
                        | None  -> infile := Some (s) ) " " ;
               match !infile, !outfile  with
                     | Some inf, Some outf -> (inf, outf)
                     | Some inf, None  -> (inf, "./abscfg/"^(String.sub inf ~pos:(11) ~len:((String.length inf) - 11)))
                     | None, Some _ -> printf "specify your input file name by -i infilename"; ("", "")
                     | _ -> printf "specify your input file name and output file name "; ("", "")
        
                     
                            let _ =
                let (infile , outfile) = parseArgs () in 
                let oc = Out_channel.create outfile in
                  let result = parse_prog infile in
                  let string_result = print_lcommand result in
                  Printf.printf "The input program is : %s" string_result;
                  print_newline();
                  let aflow = Abs.abs_flow (Seq (result, Skip)) in
                  print_abs_flow aflow;
                  print_newline();
        
                  print_abs_flow_label aflow;
                  print_newline();
        
                  print_abs_flow_constraints aflow;
                  print_newline();
                  let blocks = Cfg.blocks result in
                  let _ =  Printf.fprintf oc "%d\n" (List.length blocks + 1)  in 
                  print_out_abs_flow_edges oc aflow;
                  Printf.fprintf oc "\n";
                  print_out_abs_flow oc aflow;
                  Out_channel.close oc

      