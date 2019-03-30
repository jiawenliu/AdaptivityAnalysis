(******************************************************************************)
(*     Alt-Ergo: The SMT Solver For Software Verification                     *)
(*     Copyright (C) 2013-2014 --- OCamlPro                                   *)
(*     This file is distributed under the terms of the CeCILL-C licence       *)
(******************************************************************************)

(******************************************************************************)
(*     The Alt-Ergo theorem prover                                            *)
(*     Copyright (C) 2006-2013                                                *)
(*     CNRS - INRIA - Universite Paris Sud                                    *)
(*                                                                            *)
(*     Sylvain Conchon                                                        *)
(*     Evelyne Contejean                                                      *)
(*                                                                            *)
(*     Francois Bobot                                                         *)
(*     Mohamed Iguernelala                                                    *)
(*     Stephane Lescuyer                                                      *)
(*     Alain Mebsout                                                          *)
(*                                                                            *)
(*   This file is distributed under the terms of the CeCILL-C licence         *)
(******************************************************************************)

open Why_ptree
open Lexing
open Format
open Options

module SAT = (val (Sat_solvers.get_current ()) : Sat_solvers.S)
module FE = Frontend.Make (SAT)

let () = 
  Sys.set_signal Sys.sigint 
    (Sys.Signal_handle 
       (fun _ -> print_endline "User wants me to stop."; exit 1))	  

let pruning = 
  List.map
      (fun d -> 
        if select () > 0 then Pruning.split_and_prune (select ()) d 
        else [List.map (fun f -> f,true) d])

let processing report declss = 
  SAT.start ();
  let declss = List.map (List.map fst) declss in
  List.iter
    (List.iter 
       (fun dcl ->
	 let cnf = Cnf.make dcl in 
	 ignore (Queue.fold (FE.process_decl report)
		   (SAT.empty (), true, Explanation.empty) cnf)
       )) (pruning declss)

let _ =
  Frontend.Time.set_timeout ();
  (*Options.parse_args ();*)
  let file = get_file () in
  let cin = if file <> "" then open_in file else stdin in
  let lb = from_channel cin in 
  try 
    let d, status = FE.open_file lb cin in 
    processing FE.print_status d;
    Frontend.Time.unset_timeout ();
  with
    | Why_lexer.Lexical_error s -> 
      Loc.report err_formatter (lexeme_start_p lb, lexeme_end_p lb);
      eprintf "lexical error: %s\n@." s;
      exit 1
    | Parsing.Parse_error ->
      let  loc = (lexeme_start_p lb, lexeme_end_p lb) in
      Loc.report err_formatter loc;
      eprintf "syntax error\n@.";
      exit 1
    | Errors.Error(e,l) -> 
      Loc.report err_formatter l; 
      eprintf "typing error: %a\n@." Errors.report e;
      exit 1

