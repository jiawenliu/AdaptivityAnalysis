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

module Time = struct

  open Unix
    
  let u = ref 0.0
    
  let start () = u:=(times()).tms_utime

  let get () = 
    let res = (times()).tms_utime -. !u in
    start();
    res

  let set_timeout () =
    if timelimit () <> 0. then
      ignore (Unix.setitimer Unix.ITIMER_REAL
		{ Unix.it_value = timelimit (); Unix.it_interval = 0. })
	
  let unset_timeout () =
    if timelimit () <> 0. then
      ignore (Unix.setitimer Unix.ITIMER_REAL
		{ Unix.it_value = 0.; Unix.it_interval = 0. })

end


module type S = sig

  type sat_env

  type output = Unsat of Explanation.t | Inconsistent 
	        | Sat of sat_env | Unknown of sat_env

  val process_decl:
    (Why_ptree.sat_tdecl -> output -> int64 -> 'a) ->
    sat_env * bool * Explanation.t -> sat_tdecl ->
    sat_env * bool * Explanation.t

  val open_file: 
    Lexing.lexbuf -> in_channel ->
    ((int tdecl, int) annoted * Why_typing.env) list list * Smt_ast.status

  val print_status : sat_tdecl -> output -> int64 -> unit
end

module Make(SAT : Sat_solvers.S) : S with type sat_env = SAT.t = struct

  type sat_env = SAT.t

  type output = Unsat of Explanation.t | Inconsistent 
	        | Sat of sat_env | Unknown of sat_env

  let check_produced_proof dep =
    if verbose () then 
      fprintf fmt "checking the proof:\n-------------------\n%a@." 
        Explanation.print_proof dep;

    try
      let pb = Formula.Set.elements (Explanation.formulas_of dep) in
      let env = 
        List.fold_left
          (fun env f -> 
            SAT.assume env 
	      {Formula.f=f;
               age=0;
               lem=None;
               mf=false;
	       gf=false; 
               from_terms = []
              }
          ) (SAT.empty ()) pb
      in
      ignore (SAT.unsat 
                env 
    	        {Formula.f=Formula.vrai;
                 age=0;
                 lem=None;
                 mf=false;
	         gf=false; 
                 from_terms = []
                });
      fprintf fmt "Checking produced proof failed!@.";
      fprintf fmt "this may be due to a bug.@.";
      exit 1
    with 
      | SAT.Unsat _  -> ()
      | (SAT.Sat _ | SAT.I_dont_know _) as e -> raise e


  let do_save_used_context env dep =
    let used, unused = SAT.retrieve_used_context env dep in
    let f = Options.get_used_context_file () in
    let cout = open_out f in
    List.iter (fun s -> output_string cout (sprintf "%s\n" s)) used;
    close_out cout

  let process_decl print_status (env, consistent, dep) d =
    try
      match d.st_decl with
        | Assume(f, mf) -> 
	  SAT.assume env 
	    {Formula.f=f;
             age=0;
             lem=None; 
	     mf=mf;
             gf=false;
             from_terms = []
            },
	  consistent, dep

        |	PredDef f -> 
	  SAT.pred_def env f , consistent, dep

        | RwtDef r -> assert false

        | Query(n, f, lits, sort) ->
	  let dep = 
	    if consistent then
	      let dep' = SAT.unsat env 
	        {Formula.f=f;
                 age=0;
                 lem=None;
	         mf=(sort <> Check);
                 gf=true;
                 from_terms = []
                } in
	      Explanation.union dep' dep
	    else dep
          in
          if debug_proof () then check_produced_proof dep;
          if save_used_context () then do_save_used_context env dep;
	  print_status d (Unsat dep) (SAT.stop ());
	  env, consistent, dep
    with 
      | SAT.Sat t -> 
        print_status d (Sat t) (SAT.stop ());
        if model () then SAT.print_model ~header:true std_formatter t;
        env , consistent, dep
      | SAT.Unsat dep' -> 
        let dep = Explanation.union dep dep' in
        if debug_proof () then check_produced_proof dep;
        print_status d Inconsistent (SAT.stop ());
        env , false, dep
      | SAT.I_dont_know t -> 
        print_status d (Unknown t) (SAT.stop ());
        if model () then SAT.print_model ~header:true std_formatter t;
        env , consistent, dep

  exception Parse_only

  let open_file lb cin =
    let file = Options.get_file() in
    let d ,status =
      if smtfile() then begin
        let bname,l,status = Smt_parser.benchmark Smt_lex.token lb in
        if verbose () then printf "converting smt file : ";
        let l = List.flatten (List.map Smt_to_why.bench_to_why l) in
        if verbose () then printf "done.@.";
        if parse_only () then exit 0;
        let ltd, typ_env = Why_typing.file false Why_typing.empty_env l in
        let lltd = Why_typing.split_goals ltd in
        lltd, status
      end
      else if smt2file() then begin
        let commands = Smtlib2_parse.main Smtlib2_lex.token lb in
        if verbose () then printf "converting smt2 file : ";
        let l = Smtlib2_to_why.smt2_to_why commands in
        if verbose () then printf "done.@.";
        if parse_only () then exit 0;
        let ltd, typ_env = Why_typing.file false Why_typing.empty_env l in
        let lltd = Why_typing.split_goals ltd in
        lltd, Smt_ast.Unknown
      end
      else
        let a = Why_parser.file Why_lexer.token lb in
        if parse_only () then exit 0;
        let ltd, typ_env = Why_typing.file false Why_typing.empty_env a in
        let lltd = Why_typing.split_goals ltd in
        lltd, Smt_ast.Unknown
    in
    if file <> " stdin" then close_in cin;
    if type_only () then exit 0;
    d, status

  let print_status d s steps =
    let satmode = smtfile() || smt2file() || satmode() in
    match s with
      | Unsat dep -> 
        if not satmode then Loc.report std_formatter d.st_loc;
        if satmode then printf "@{<C.F_Red>unsat@}@." 
        else printf "@{<C.F_Green>Valid@} (%2.4f) (%Ld)@." (Time.get()) steps;
        if proof () && not (debug_proof ()) && not (save_used_context ()) then 
          printf "Proof:\n%a@." Explanation.print_proof dep
	    
      | Inconsistent ->
        if not satmode then 
	  (Loc.report std_formatter d.st_loc; 
	   fprintf fmt "Inconsistent assumption@.")
        else printf "unsat@."
	  
      | Unknown t ->
        if not satmode then
	  (Loc.report std_formatter d.st_loc; printf "I don't know.@.")
        else printf "unknown@."
	  
      | Sat t -> 
        if not satmode then Loc.report std_formatter d.st_loc;
        if satmode then printf "unknown (sat)@." 
        else printf "I don't know.@."
end
