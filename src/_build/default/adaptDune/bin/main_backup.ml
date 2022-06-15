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
                     | Some inf, None  -> (inf, "./graphs/"^(String.sub inf ~pos:(11) ~len:((String.length inf) - 11)))
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

                  open Core
                  open Syntax
                  open Format
                  
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
                               | Some _, None  -> printf "specify your output file name by -o outfilename"; ("", "")
                               | None, Some _ -> printf "specify your input file name by -i infilename"; ("", "")
                               | _ -> printf "specify your input file name and output file name "; ("", "")
                  
                  (* 
                  let parseArgs () =  
                          Arg.parse argDefs 
                          (fun s -> 
                                  match !infile  with 
                                        | Some (_) -> printf "%s" "specify "  
                                        | None  -> infile := Some (s) ) " " ;
                               match !infile  with
                                     | Some inf -> inf
                                     | None -> printf "specify your input file name by -i infilename"; ""
                                      *)
                  let parse_prog file =
                      let ic = In_channel.create file
                      in
                        let lb = (Lexing.from_channel ic) in 
                          Parser.toplevel Lexer.main lb
                  
                  let _ =
                      (* let lexbuf = Lexing.from_channel stdin in *)
                      let (infile , outfile) = parseArgs () in 
                      let oc = Out_channel.create outfile in
                        let result = parse_prog infile in
                        let cfg_result = Cfg.generate_cfg result in 
                        let blocks = cfg_result.nodes in
                        let _ =  Printf.fprintf oc "%d\n" (List.length blocks) in 
                        let blocksmap = cfg_result.node_map in
                        Int.Map.iter_keys blocksmap
                        ~f: (fun k -> let node = Int.Map.find blocksmap k in
                         match node with
                         | Some bk ->   Printf.printf "key %d, node is %s ; \n" k (Syntax.print_block bk)
                        | None ->   Printf.printf " No node for key %d; \n" k
                         ) ;
                        List.fold_left ~f:( fun () block -> Printf.fprintf oc "%d," (Syntax.isQuery block) ) ~init:() blocks;
                        Printf.fprintf oc "\n";
                        let string_result = print_lcommand result in
                        Printf.printf "The input program is : %s" string_result;
                        let flow = cfg_result.edges in
                        let precessor_map = cfg_result.pre_map in
                        Int.Map.iter_keys precessor_map
                        ~f: (fun k -> let node = Int.Map.find precessor_map k in
                         match node with
                         | Some precessors_labels ->  List.fold_left precessors_labels ~init:() 
                         ~f: (fun () precessor -> Printf.printf "key %d, precessor is %d ; \n" k (Syntax.print_label precessor) )
                        | None ->   Printf.printf " No node for key %d; \n" k
                         ) ;
                         let successor_map = cfg_result.suc_map in
                        Int.Map.iter_keys successor_map
                        ~f: (fun k -> let node = Int.Map.find successor_map k in
                         match node with
                         | Some suc_labels ->  List.fold_left suc_labels ~init:() 
                         ~f: (fun () suc -> Printf.printf "key %d, successor is %d ; \n" k (Syntax.print_label suc) )
                        | None ->   Printf.printf " No node for key %d; \n" k
                         ) ;
                        print_flow flow;
                        print_out_flow oc flow;
                        let kill_result = Df.kill result (List.nth_exn blocks 1) in
                        Printf.printf "kill of 2nd block with size : %d \n" (List.length kill_result) ;
                        List.fold_left ~f:( fun () (x, v) -> 
                          Printf.printf "%s : %d\n" x v ) ~init:() kill_result;
                        print_newline();
                        let in_init = Df.in_init result in
                        List.fold_left ~f:( fun () (x, v) -> 
                          Printf.printf "%s : %d\n" x v ) ~init:() in_init;
                        let cfg_result = Cfg.generate_cfg result in 
                        let _ =  Printf.printf  "%d\n" (List.length cfg_result.nodes ) in   
                        Out_channel.close oc
                          
                  
                        