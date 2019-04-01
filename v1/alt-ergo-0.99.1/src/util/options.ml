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

let fmt = Format.err_formatter

module M = struct

  let file = ref ""
  let session_file = ref ""
  let used_context_file = ref ""
  let rewriting = ref false
  let type_only = ref false
  let parse_only = ref false
  let steps_bound = ref (-1)
  let age_bound = ref 10
  let debug = ref false
  let notriggers = ref false
  let debug_cc = ref false
  let debug_use = ref false
  let debug_arrays = ref false
  let debug_uf = ref false
  let debug_sat = ref false
  let debug_sat_simple = ref false
  let debug_typing = ref false
  let debug_constr = ref false
  let verbose = ref false
  let debug_fm = ref false
  let debug_sum = ref false
  let debug_arith = ref false
  let debug_combine = ref false
  let debug_bitv = ref false
  let debug_ac = ref false
  let debug_split = ref false
  let options = ref false
  let smtfile = ref false
  let smt2file = ref false
  let satmode = ref false
  let greedy = ref false
  let triggers_var = ref false
  let nb_triggers = ref 2
  let enable_assertions = ref false
  let select = ref 0
  let no_rm_eq_existential = ref false
  let no_Ematching = ref false
  let nocontracongru = ref false
  let term_like_pp = ref true
  let debug_types = ref false 
  let all_models = ref false
  let model = ref false
  let complete_model = ref false
  let proof = ref false
  let debug_proof = ref false
  let rules = ref (-1)
  let max_split = ref (Numbers.Q.of_int 1000000)
  let restricted = ref false
  let bottom_classes = ref false
  let timelimit = ref 0.
  let debug_matching = ref false
  let sat_plugin = ref ""
  let normalize_instances = ref false
  let partial_bmodel = ref true
    
  let show_version () = Format.printf "%s@." Version.version; exit 0
  let show_libdir () = Format.printf "%s@." Version.libdir; exit 0

  let set_max_split s = 
    max_split := 
      try Numbers.Q.of_string s 
      with Failure _ -> Numbers.Q.m_one

  let set_sat_plugin s = sat_plugin := s

  let set_proof b = proof := b

  let set_rules = function
    | "parsing" -> rules := 0
    | "typing" -> rules := 1
    | "sat" -> rules := 2
    | "cc" -> rules := 3
    | "arith" -> rules := 4
    | _ -> rules := -1

  let set_limit t =
    match Sys.os_type with
      | "Win32" -> Format.eprintf "timelimit not supported on Win32 (ignored)@."
      | _ -> timelimit := t

  let replay = ref false
  let replay_used_context = ref false
  let replay_all_used_context = ref false
  let save_used_context = ref false

  let replay_satml_dfs = ref false

  let usage = "usage: alt-ergo [options] file.<mlw|smt>"

  let spec = [
    "-rwt", Arg.Set rewriting, " use rewriting instead of axiomatic approach";
    "-parse-only", Arg.Set parse_only, " stop after parsing";
    "-type-only", Arg.Set type_only , " stop after typing";
    "-notriggers", Arg.Set notriggers, "  trigger inference";
    "-debug", Arg.Set debug, "  sets the debugging flag";
    "-dcc", Arg.Set debug_cc, "  sets the debugging flag of cc";
    "-duse", Arg.Set debug_use, "  sets the debugging flag of use";
    "-duf", Arg.Set debug_uf, "  sets the debugging flag of uf";
    "-dfm", Arg.Set debug_fm, "  sets the debugging flag of inequalities";
    "-dsum", Arg.Set debug_sum, "  sets the debugging flag of Sum";
    "-darith", Arg.Set debug_arith, 
    " sets the debugging flag of Arith (without fm)";
    "-dbitv", Arg.Set debug_bitv, "  sets the debugging flag of bitv";
    "-dac", Arg.Set debug_ac, "  sets the debugging flag of ac";
    "-dsat", Arg.Set debug_sat, "  sets the debugging flag of sat";
    "-dsats", Arg.Set debug_sat_simple, 
    "  sets the debugging flag of sat (simple output)";
    "-dtyping", Arg.Set debug_typing, "  sets the debugging flag of typing";
    "-types", Arg.Set debug_types, "  sets the debugging flag of types";
    "-dconstr", Arg.Set debug_constr, 
    "  sets the debugging flag of constructors";
    "-darrays", Arg.Set debug_arrays, "  sets the debugging flag of arrays";
    "-dcombine", Arg.Set debug_combine, "  sets the debugging flag of combine";
    "-dsplit", Arg.Set debug_split, "  sets the debugging flag of case-split analysis";
    "-dmatching", Arg.Set debug_matching, "  sets the debugging flag of E-matching";
    "-v", Arg.Set verbose, "  sets the verbose mode";
    "-version", Arg.Unit show_version, "  prints the version number";
    "-where", Arg.Unit show_libdir, "  prints the directory of the library";
    "-steps-bound", Arg.Set_int steps_bound, " <n> set the maximum number of steps";
    "-age-bound", Arg.Set_int age_bound, " <n> set the age limite bound";
    "-sat-mode" , Arg.Set satmode , " mode sat/unsat";
    "-greedy" , Arg.Set greedy, 
    " use all available ground terms in instanciation";
    "-nb-triggers" , Arg.Set_int nb_triggers, 
    " number of redondant (multi)triggers (default: 2)";
    "-select" , Arg.Set_int select, 
    "k tries to select relevant (at level k) hypotheses for each goal";
    "-triggers-var" , Arg.Set triggers_var , " allows variables as triggers";
    "-no-rm-eq-existential", Arg.Set no_rm_eq_existential, " does not substitute a variable in an existential when an equality gives the value of the variable";
    "-no-Ematching", Arg.Set no_Ematching, " disable matching modulo ground equalities";
    "-nocontracongru", Arg.Set nocontracongru, "";
    "-term-like-pp", Arg.Set term_like_pp, "Output semantic values as terms";
    "-all-models", Arg.Set all_models, "experimental support for all models";
    "-model", Arg.Set model, "experimental support for models on labeled terms";
    "-complete-model", Arg.Set complete_model, "experimental support for complete model";
    "-proof", Arg.Set proof, "experimental support for succint proof";
    "-debug-proof", Arg.Set debug_proof, "replay insatisfiable core produced by -proof. This options implies -proof";
    "-rules", Arg.String set_rules, "<parsing|typing|sat|cc|arith> output rules used on stderr";
    "-max-split", Arg.String set_max_split,
    (Format.sprintf " maximum size of case-split (default value : %s)" 
       (Numbers.Q.string_of !max_split));
    "-restricted", Arg.Set restricted, 
    " restrict set of decision procedures (equality, arithmetic and AC)";
    "-bottom-classes", Arg.Set bottom_classes, "show equivalence classes at each bottom of the sat";
    "-replay", Arg.Set replay, "replay session saved in .agr";
    "-replay-used-context", Arg.Set replay_used_context, "replay with axioms and predicates saved in .used file";
    "-replay-all-used-context", Arg.Set replay_all_used_context, "replay with all axioms and predicates saved in .used files of the current directory";
    "-save-used-context", Arg.Set save_used_context, "save used axioms and predicates in a .used file. This options implies -proof";
    "-replay-satml-dfs", Arg.Set replay_satml_dfs, "Debug option for the satML plugin. Replays proven (valid) goals (with generated ground instances) using Dfs-sat";
    "-timelimit", Arg.Float set_limit, "n set the time limit to n seconds (not supported on Windows)";
    "-sat-plugin" , Arg.String set_sat_plugin, 
    "Use the given SAT-solver instead of the default DFS-based SAT solver"; 
    "-normalize-instances" , Arg.Set normalize_instances, 
    "Normalize generated ground instances w.r.t. the state of the theory"; 
  ]

  let profiling = ref false
  let thread_yield = ref (fun () -> ())
  let (timer_start : (Timers.kind -> unit) ref) = ref (fun _ -> ())
  let (timer_pause : (Timers.kind -> unit) ref) = ref (fun _ -> ())
  let (timeout : (unit -> unit) ref) =
    ref (fun () -> Format.printf "Timeout@."; exit 142)

end

let parse_args () =
  let ofile = ref None in
  let set_file s =
    if Filename.check_suffix s ".mlw" || Filename.check_suffix s ".why"
    then ofile := Some s
    else
      if Filename.check_suffix s ".smt"
      then begin 
	M.smtfile := true ;
        ofile := Some s
      end
      else
	if Filename.check_suffix s ".smt2"
	then begin 
	  M.smt2file := true; 
          ofile := Some s
	end
	else raise (Arg.Bad "no .mlw, .smt or smt2 extension")
  in
  Arg.parse M.spec set_file M.usage;
  match !ofile with 
    | Some f -> 
      M.file := f;
      M.session_file := (Filename.chop_extension f)^".agr";
      M.used_context_file := (Filename.chop_extension f)^".used"
    | None ->
      M.smt2file := true

(* 
   parse_args () should be called here because some choices
   during compilation depend on given options
   (e.g. dfs-sat
*)
let _ = parse_args ()


(** setter functions **********************************************************)

(** setters for debug flags *)
let set_debug b = M.debug := b
let set_debug_cc b = M.debug_cc := b
let set_debug_use b = M.debug_use := b
let set_debug_uf b = M.debug_uf := b
let set_debug_fm b = M.debug_fm := b
let set_debug_sum b = M.debug_sum := b
let set_debug_arith b = M.debug_arith := b
let set_debug_bitv b = M.debug_bitv := b
let set_debug_ac   b = M.debug_ac := b
let set_debug_sat b = M.debug_sat := b
let set_debug_sat_simple b = M.debug_sat_simple := b
let set_debug_typing b = M.debug_typing := b
let set_debug_constr b = M.debug_constr := b
let set_debug_arrays b = M.debug_arrays := b
let set_debug_types b = M.debug_types := b
let set_debug_combine b = M.debug_combine := b
let set_debug_proof b = M.debug_proof := b
let set_debug_split b = M.debug_split := b
let set_debug_matching b = M.debug_matching := b

(** additional setters *)
let set_type_only b = M.type_only := b
let set_parse_only b = M.parse_only := b
let set_steps_bound b = M.steps_bound := b
let set_age_bound b = M.age_bound := b
let set_notriggers b = M.notriggers := b
let set_verbose b = M.verbose := b
let set_greedy b = M.greedy := b
let set_triggers_var b = M.triggers_var := b
let set_nb_triggers b = M.nb_triggers := b
let set_select b = M.select := b
let set_no_rm_eq_existential b = M.no_rm_eq_existential := b
let set_no_Ematching b = M.no_Ematching := b
let set_nocontracongru b = M.nocontracongru := b
let set_term_like_pp b = M.term_like_pp := b
let set_all_models b = M.all_models := b
let set_model b = M.model := b
let set_complete_model b = M.complete_model := b
let set_max_split b = M.max_split := b
let set_rewriting b = M.rewriting := b
let set_proof b = M.proof := b
let set_rules b = M.rules := b
let set_restricted b = M.restricted := b
let set_bottom_classes b = M.bottom_classes := b
let set_timelimit b = M.timelimit := b
let set_profiling b = M.profiling := b
let set_thread_yield f = M.thread_yield := f
let set_timer_start f = M.timer_start := f
let set_timer_pause f = M.timer_pause := f
let set_timeout f = M.timeout := f
let set_partial_bmodel b = M.partial_bmodel := b

(** getter functions **********************************************************)

(** getters for debug flags *)
let debug () = !M.debug
let debug_cc () = !M.debug_cc
let debug_use () = !M.debug_use
let debug_uf () = !M.debug_uf
let debug_fm () = !M.debug_fm
let debug_sum () = !M.debug_sum
let debug_arith () = !M.debug_arith
let debug_bitv () = !M.debug_bitv
let debug_ac   () = !M.debug_ac
let debug_sat () = !M.debug_sat
let debug_sat_simple () = !M.debug_sat_simple
let debug_typing () = !M.debug_typing
let debug_constr () = !M.debug_constr
let debug_arrays () = !M.debug_arrays
let debug_types () = !M.debug_types
let debug_combine () = !M.debug_combine
let debug_proof () = !M.debug_proof
let debug_split () = !M.debug_split
let debug_matching () = !M.debug_matching

(** additional getters *)
let type_only () = !M.type_only
let parse_only () = !M.parse_only
let steps_bound () = !M.steps_bound
let age_bound () = !M.age_bound
let notriggers () = !M.notriggers
let verbose () = !M.verbose
let greedy () = !M.greedy
let triggers_var () = !M.triggers_var
let nb_triggers () = !M.nb_triggers
let select () = !M.select
let no_rm_eq_existential () = !M.no_rm_eq_existential
let no_Ematching () = !M.no_Ematching
let nocontracongru () = !M.nocontracongru
let term_like_pp () = !M.term_like_pp
let all_models () = !M.all_models
let model () = !M.model || !M.complete_model
let complete_model () = !M.complete_model
let max_split () = !M.max_split
let rewriting () = !M.rewriting
let proof () = !M.proof || !M.save_used_context || !M.debug_proof
let rules () = !M.rules
let restricted () = !M.restricted
let bottom_classes () = !M.bottom_classes
let timelimit () = !M.timelimit
let enable_assertions () = !M.enable_assertions
let profiling () = !M.profiling

let replay () = !M.replay
let replay_used_context () = !M.replay_used_context
let replay_all_used_context () = !M.replay_all_used_context
let save_used_context () = !M.save_used_context
let replay_satml_dfs () = !M.replay_satml_dfs
let get_file () = !M.file
let get_session_file () = !M.session_file
let get_used_context_file () = !M.used_context_file
let satmode () = !M.satmode
let smt2file () = !M.smt2file
let smtfile () = !M.smtfile
let sat_plugin () = !M.sat_plugin
let normalize_instances () = !M.normalize_instances
let partial_bmodel () = !M.partial_bmodel

(** particular getters : functions that are immediately executed **************)
let exec_thread_yield () = !M.thread_yield ()
let exec_timer_start kd = !M.timer_start kd
let exec_timer_pause kd = !M.timer_pause kd
let exec_timeout () = !M.timeout ()

let tool_req n msg = 
  if rules () = n then Format.fprintf fmt "[rule] %s@." msg
