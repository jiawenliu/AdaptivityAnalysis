(* * ******************  ADAPTIVITY ANALYSIS: ****************** *)

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

let parseArgs () =  
  Arg.parse argDefs 
  (fun s -> 
          match !infile  with 
                | Some (_) -> printf "%s" "specify "  
                | None  -> infile := Some (s) ) " " ;
       match !infile, !outfile  with
             | Some inputfile, Some outf -> (inputfile, outf)
             (* | Some _, None  -> printf "specify your output file name by -o outfilename"; ("", "") *)
             | Some inputfile, None  -> (inputfile, inputfile)
             | None, Some _ -> printf "specify your input file name by -i infilename"; ("", "")
             | _ -> printf "specify your input file name and output file name "; ("", "")


let parse_prog file =
    let ic = In_channel.create file
    in
      let lb = (Lexing.from_channel ic) in 
        Parser.toplevel Lexer.main lb

let parse_program_name filepath = 
      match List.hd (List.rev (String.split filepath ~on: '/' )) with
      | Some prog -> prog
      | None -> "tmp_program_name"
let _ =
    (* PARSE PROGRAM NAME *)
      let (infile , outfile) = parseArgs () in 
      let prog_name = parse_program_name outfile in

    (* PARSE PROGRAM AND GENERATE THE BASIC CONTROL FLOW GRAPH *)
      let result = parse_prog infile in
      let cfg_result = Cfg.generate_cfg result in 
      let blocks = cfg_result.nodes in

    (******************** DATA FLOW GRAPH VERSION - ADPTFUN ********************)    
      let t = Caml_unix.gettimeofday () in
      let outfile_dfg = "./dfg/"^prog_name in
      let dfg_output_channel = Out_channel.create outfile_dfg in
      let _ =  Printf.fprintf dfg_output_channel "%d\n" (List.length blocks) in 
      let _ = cfg_result.node_map in
      List.fold_left ~f:( fun () block -> Printf.fprintf dfg_output_channel "%d," (Syntax.isQuery block) ) ~init:() blocks;
      let string_result = print_lcommand result in
      Printf.printf "The input program is : %s" string_result;
      let _ = Df.kill result (List.nth_exn blocks 1) in
      print_newline();
      let cfg_result = Cfg.generate_cfg result in 
      let _ =  Printf.printf  "%d\n" (List.length cfg_result.nodes ) in   
      let (_, rd_in) = Df.kildall cfg_result in
      let dfg_result =Dfg.ddg result rd_in in
      Printf.printf "computation of the DFG total time:%fs\n" (Caml_unix.gettimeofday () -. t) ;
      Printf.fprintf dfg_output_channel "\n";
      print_out_dcdg dfg_output_channel dfg_result;    
      (*add weight line **) 
      Printf.fprintf dfg_output_channel "\n";
      (* Close Channel *)
      Out_channel.close dfg_output_channel;  


      (******************** DATA AND CONTROL FLOW GRAPH VERSION - ADPTFUN  ********************)
      let outfile_dcfg = "./dcfg/"^prog_name in
      let oc = Out_channel.create outfile_dcfg in
      let _ =  Printf.fprintf oc "%d\n" (List.length blocks) in 
      let _ = cfg_result.node_map in
      List.fold_left ~f:( fun () block -> Printf.fprintf oc "%d," (Syntax.isQuery block) ) ~init:() blocks;
      let _ = Df.kill result (List.nth_exn blocks 1) in
      print_newline();      
      let cfg_result = Cfg.generate_cfg result in 
      let _ =  Printf.printf  "%d\n" (List.length cfg_result.nodes ) in   
      let (_, rd_in) = Df.kildall cfg_result in
      Printf.printf "DCDG result:\n";
      let dcdg_result =Dcfg.dcdg result cfg_result rd_in in
      Printf.printf "computation of the DCDG total time:%fs\n" (Caml_unix.gettimeofday () -. t) ;
      Printf.fprintf oc "\n";
      print_out_dcdg oc dcdg_result;    
        (**** add weight line ****) 
      Printf.fprintf oc "\n";
        (* Weight_infer.print_lcom_list result ; *)
      let weight_list  = Weight_infer.infer result oc blocks in 
      Weight_infer.print_weight_list  weight_list ;
        (* Close Channel *)
      Out_channel.close oc;

      (******************** ABSCFG  ********************)
      let time_abscfg = Caml_unix.gettimeofday () in
      let outfile_abscfg = "./abscfg/"^prog_name in 
      let oc = Out_channel.create outfile_abscfg in
      Printf.printf "ABSCFG result:\n";
      let aflow = Abs.abs_flow (Seq (result, (Skip (Label (-1))))) in
      let blocks = Cfg.blocks result in
      let _ =  Printf.fprintf oc "%d\n" (List.length blocks + 1)  in 
      print_out_abs_flow_edges oc aflow;
      Printf.fprintf oc "\n";
      print_out_abs_flow oc aflow;
      Printf.printf "computation of the abscfg total time:%fs\n" (Caml_unix.gettimeofday () -. time_abscfg) ;
      Printf.printf "computation of the total parsing and graph generation time:%fs\n" (Caml_unix.gettimeofday () -. t) ;
      (* Close Channel *)
      Out_channel.close oc