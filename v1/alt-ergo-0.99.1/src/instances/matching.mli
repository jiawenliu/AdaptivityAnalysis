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
  type tbox

  val empty : t
  val add_terms : t -> Term.Set.t -> Formula.gformula -> t
  val add_lemma : t -> Formula.gformula -> Explanation.t -> t
  val add_predicate : t -> Formula.gformula -> t

  type instances = (Formula.gformula * Explanation.t) list

  val m_lemmas :
    t ->
    tbox ->
    (Formula.t -> Formula.t -> bool) ->
    instances * instances (* goal_directed, others *)

  val m_predicates :
    t ->
    tbox ->
    (Formula.t -> Formula.t -> bool) ->
    instances * instances (* goal_directed, others *)

  (* returns names of used axioms/predicates * unused axioms/predicates *)
  val retrieve_used_context : t -> Explanation.t -> string list * string list

end

module Make (X : Theory.S) : S with type tbox = X.t
