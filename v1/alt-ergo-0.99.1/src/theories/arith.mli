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

module Type (X : Sig.X ): Polynome.T with type r = X.r

module type EXTENDED_Polynome = sig
  include Polynome.T
  val extract : r -> t option
  val embed : t -> r
end

module Shostak 
  (X : Sig.X)
  (P : EXTENDED_Polynome with type r = X.r) : Sig.SHOSTAK
  with type r = X.r and type t = P.t

module Relation
  (X : Sig.X)
  (Uf : Uf.S)
  (P : EXTENDED_Polynome with type r = X.r) 
  : Sig.RELATION 
  with type r = X.r and type uf = Uf.t
