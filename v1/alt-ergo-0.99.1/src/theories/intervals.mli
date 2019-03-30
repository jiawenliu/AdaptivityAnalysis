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

type t

exception NotConsistent of Explanation.t

val undefined : Ty.t -> t

val point : Numbers.Q.t -> Ty.t -> Explanation.t -> t

val doesnt_contain_0 : t -> Sig.answer

val is_strict_smaller : t -> t -> bool

val new_borne_sup : Explanation.t -> Numbers.Q.t -> is_le : bool -> t -> t

val new_borne_inf : Explanation.t -> Numbers.Q.t -> is_le : bool -> t -> t

val is_point : t -> (Numbers.Q.t * Explanation.t) option

val intersect : t -> t -> t

val exclude : t -> t -> t

val mult : t -> t -> t

val power : int -> t -> t

val sqrt : t -> t

val root : int -> t -> t 

val add : t -> t -> t

val scale : Numbers.Q.t -> t -> t

val pretty_print : Format.formatter -> t -> unit
val print : Format.formatter -> t -> unit

val finite_size : t -> Numbers.Q.t option

val borne_inf : t -> Numbers.Q.t * Explanation.t

val div : t -> t -> t
