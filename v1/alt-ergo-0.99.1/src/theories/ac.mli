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

  (* the type of amalgamated AC semantic values *)
  type r 

  (* the type of AC semantic values used by the theory *)
  type t = r Sig.ac
      
  (* builds an embeded semantic value from an AC term *)
  val make : Term.t -> r * Literal.LT.t list

  (* tells whether the given term is AC*)
  val is_mine_symb : Symbols.t -> bool

  (* compares two AC semantic values *)
  val compare : t -> t -> int

  (* hash function for ac values *)
  val hash : t -> int

  (* returns the type infos of the given term *)
  val type_info : t -> Ty.t

  (* prints the AC semantic value *)
  val print : Format.formatter -> t -> unit
    
  (* returns the leaves of the given AC semantic value *)
  val leaves : t -> r list

  (* replaces the first argument by the second one in the given AC value *)
  val subst : r -> r -> t -> r

  (* add flatten the 2nd arg w.r.t HS.t, add it to the given list 
     and compact the result *)
  val add : Symbols.t -> r * int -> (r * int) list -> (r * int) list

  val fully_interpreted : Symbols.t -> bool

  val abstract_selectors : t -> (r * r) list -> r * (r * r) list
    
  val compact : (r * int) list -> (r * int) list

end

module Make (X : Sig.X) : S with type r = X.r
