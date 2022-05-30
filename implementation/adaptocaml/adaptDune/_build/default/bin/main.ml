open Syntax
open Format

let infile = ref (None : string option)
(* let outfile = ref (None : string option) *)



let argDefs = [
    "-i", Arg.String (fun s -> infile := Some s  ), "specify the input file name" ] 
    (* "-o", Arg.String (fun s -> outfile := Some s ), "specify the output file name" 
] *)

let parseArgs () =  
        Arg.parse argDefs 
        (fun s -> 
                match !infile  with 
                      | Some (_) -> printf "%s" "specify "  
                      | None  -> infile := Some (s) ) " " ;
             match !infile  with
                   | Some inf -> inf
                   | None -> printf "specify your input file name by -i infilename"; ""
                   
let parse_prog file =
    let ic = open_in file 
    in
      let lb = (Lexing.from_channel ic) in 
        Parser.toplevel Lexer.main lb

let _ =
    (* let lexbuf = Lexing.from_channel stdin in *)
    let infile  = parseArgs () in 
      let result = parse_prog infile in
      let final_label = Cfg.final result in 
      let _ =  Printf.printf "final label size: %d" (List.length final_label) in 
        List.fold_left ( fun () label -> Printf.printf "final label : %d" (Syntax.print_label label) ) () final_label;
      let blocks = Cfg.blocks result in
      List.fold_left ( fun () block -> Printf.printf "%s ; \n" (Syntax.print_block block) ) () blocks;
      let string_result = print_lcommand result in
      Printf.printf "The input program is : %s" string_result;
      let flow = Cfg.flow result in
      print_flow flow;
      print_newline();
        

      