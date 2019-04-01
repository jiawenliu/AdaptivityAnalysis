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

module type S = sig
  type t

  exception Sat of t
  exception Unsat of Explanation.t
  exception I_dont_know of t

  (* the empty sat-solver context *)
  val empty : unit -> t
  val empty_with_inst : (Formula.t -> bool) -> t
    
  (* [assume env f] assume a new formula [f] in [env]. Raises Unsat if
     [f] is unsatisfiable in [env] *)
  val assume : t -> Formula.gformula -> t
    
  (* [pred_def env f] assume a new predicate definition [f] in [env]. *)
  val pred_def : t -> Formula.t -> t

  (* [unsat env f size] checks the unsatisfiability of [f] in
     [env]. Raises I_dont_know when the proof tree's height reaches
     [size]. Raises Sat if [f] is satisfiable in [env] *)
  val unsat : t -> Formula.gformula -> Explanation.t

  val print_model : header:bool -> Format.formatter -> t -> unit

  val start : unit -> unit
  val stop : unit -> int64

  (* returns names of used axioms/predicates * unused axioms/predicates *)
  val retrieve_used_context : t -> Explanation.t -> string list * string list
end

(*** Dfs_sat ***)
module Dfs_sat : S 

val get_current : unit -> (module S)
(** returns the current activated SAT-solver. The default value is Dfs_sat.
    When the selected SAT-solver is an external plugin, the first call of this 
    function will attemp to dynamically load it **)

val set_current : (module S) -> unit
(** sets a new SAT-solver. This function is intended to be used by dynamically
    loaded plugins **)
