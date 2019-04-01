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

val fmt : Format.formatter

(** setter functions **********************************************************)

(** setters for debug flags *)
val set_debug : bool -> unit
val set_debug_cc : bool -> unit
val set_debug_use : bool -> unit
val set_debug_uf : bool -> unit
val set_debug_fm : bool -> unit
val set_debug_sum : bool -> unit
val set_debug_arith : bool -> unit
val set_debug_bitv : bool -> unit
val set_debug_ac : bool -> unit
val set_debug_sat : bool -> unit
val set_debug_sat_simple : bool -> unit
val set_debug_typing : bool -> unit
val set_debug_constr : bool -> unit
val set_debug_arrays : bool -> unit
val set_debug_types : bool -> unit
val set_debug_combine : bool -> unit
val set_debug_proof : bool -> unit
val set_debug_split : bool -> unit
val set_debug_matching : bool -> unit
val set_profiling : bool -> unit

(** additional setters *)

val set_type_only : bool -> unit
val set_parse_only : bool -> unit
val set_verbose : bool -> unit
val set_steps_bound : int -> unit
val set_age_bound : int -> unit
val set_notriggers : bool -> unit
val set_triggers_var : bool -> unit
val set_nb_triggers : int -> unit
val set_greedy : bool -> unit
val set_select : int -> unit
val set_no_rm_eq_existential : bool -> unit
val set_no_Ematching : bool -> unit
val set_nocontracongru : bool -> unit
val set_term_like_pp : bool -> unit
val set_all_models : bool -> unit
val set_model : bool -> unit
val set_complete_model : bool -> unit
val set_max_split : Numbers.Q.t -> unit
val set_rewriting : bool -> unit
val set_proof : bool -> unit
val set_rules : int -> unit
val set_restricted : bool -> unit
val set_bottom_classes : bool -> unit
val set_timelimit : float -> unit
val set_thread_yield : (unit -> unit) -> unit
val set_timer_start : (Timers.kind -> unit) -> unit
val set_timer_pause : (Timers.kind -> unit) -> unit
val set_timeout : (unit -> unit) -> unit
val set_partial_bmodel : bool -> unit


(** getter functions **********************************************************)

(** getters for debug flags *)
val debug : unit -> bool
val debug_cc : unit -> bool
val debug_use : unit -> bool
val debug_uf : unit -> bool
val debug_fm : unit -> bool
val debug_sum : unit -> bool
val debug_arith : unit -> bool
val debug_bitv : unit -> bool
val debug_ac : unit -> bool
val debug_sat : unit -> bool
val debug_sat_simple : unit -> bool
val debug_typing : unit -> bool
val debug_constr : unit -> bool
val debug_arrays : unit -> bool
val debug_types : unit -> bool
val debug_combine : unit -> bool
val debug_proof : unit -> bool
val debug_split : unit -> bool
val debug_matching : unit -> bool

(** additional getters *)
val enable_assertions : unit -> bool
val type_only : unit -> bool
val parse_only : unit -> bool
val steps_bound : unit -> int
val age_bound : unit -> int
val notriggers : unit -> bool
val triggers_var : unit -> bool
val nb_triggers : unit -> int
val verbose : unit -> bool
val greedy : unit -> bool
val select : unit -> int
val no_rm_eq_existential : unit -> bool
val no_Ematching : unit -> bool
val nocontracongru : unit -> bool
val term_like_pp : unit -> bool
val all_models : unit -> bool
val model : unit -> bool
val complete_model : unit -> bool
val max_split : unit -> Numbers.Q.t
val rewriting : unit -> bool
val proof : unit -> bool
val rules : unit -> int
val restricted : unit -> bool
val bottom_classes : unit -> bool
val timelimit : unit -> float
val profiling : unit -> bool
val satmode : unit -> bool
val smt2file : unit -> bool
val smtfile : unit -> bool
val replay : unit -> bool
val replay_used_context : unit -> bool
val replay_all_used_context : unit -> bool
val save_used_context : unit -> bool
val replay_satml_dfs : unit -> bool
val get_file : unit -> string
val get_session_file : unit -> string
val get_used_context_file : unit -> string
val sat_plugin : unit -> string
val normalize_instances : unit -> bool
val partial_bmodel : unit -> bool

(** particular getters : functions that are immediately executed **************)
val exec_thread_yield : unit -> unit
val exec_timer_start : Timers.kind -> unit
val exec_timer_pause : Timers.kind -> unit
val exec_timeout : unit -> unit

val tool_req : int -> string -> unit
