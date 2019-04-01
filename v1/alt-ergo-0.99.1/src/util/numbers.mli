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

module Z : sig

  type t

  val zero : t
  val one : t

  val my_gcd : t -> t -> t
  val my_lcm : t -> t -> t
  val of_string : string -> t

end

module Q : sig

  exception Not_a_float

  type t

  val zero : t
  val one : t
  val m_one : t
  val of_int : int -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val sign : t -> int
  val minus : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val div : t -> t -> t
  val modulo : t -> t -> t (* Euclidean division's remainder *)
  val power : t -> int -> t
  val abs : t -> t
  val is_integer : t -> bool
  val floor : t -> t
  val ceiling : t -> t
  val of_string : string -> t
  val string_of : t -> string
  val of_z : Z.t -> t
  val z_of : t -> Z.t
  val float_of : t -> float
  val of_float : float -> t
  val denominator : t -> Z.t
  val numerator : t -> Z.t

end
