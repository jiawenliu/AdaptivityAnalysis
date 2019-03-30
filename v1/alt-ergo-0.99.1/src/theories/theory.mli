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

  val empty : unit -> t
  val assume : Literal.LT.t -> Explanation.t -> t -> t * Term.Set.t * int
  val query : Literal.LT.t -> t -> Sig.answer
  val class_of : t -> Term.t -> Term.t list
  val are_equal : t -> Term.t -> Term.t -> Sig.answer
  val print_model : Format.formatter -> t -> unit
  val cl_extract : t -> Term.Set.t list
  val term_repr : t -> Term.t -> Term.t
  val extract_ground_terms : t -> Term.Set.t
end

module Main : S
