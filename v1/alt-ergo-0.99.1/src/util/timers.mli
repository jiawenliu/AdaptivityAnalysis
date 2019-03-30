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

type kind =
  | TNone
  | TSat
  | TMatch
  | TCC
  | TArith
  | TArrays
  | TSum
  | TRecords
  | TAc

type t

val init : unit -> t

val reset : t -> unit

val pause : t -> kind -> unit

val update : t -> unit

val pause_all : t -> unit

val start : t -> kind -> unit

val pause_and_restart : t -> kind -> (unit -> unit) -> unit

val get : t -> kind -> float
