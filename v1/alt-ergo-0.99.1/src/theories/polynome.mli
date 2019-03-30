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

open Numbers.Z
open Numbers.Q

exception Not_a_num
exception Maybe_zero

module type S = sig
  type r 
  val compare : r -> r-> int
  val term_embed : Term.t -> r
  val mult : r -> r -> r
  val print : Format.formatter -> r -> unit
  val abstract_selectors : r -> (r * r) list -> r * (r * r) list
end

module type T = sig

  type r 
  type t
    
  val compare : t -> t -> int
  val hash : t -> int

  val create : (Numbers.Q.t * r) list -> Numbers.Q.t -> Ty.t-> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mult : t -> t -> t
  val mult_const : Numbers.Q.t -> t -> t
  val div : t -> t -> t * bool
  val modulo : t -> t -> t

  val is_const : t -> Numbers.Q.t option
  val is_empty : t -> bool
  val find : r -> t -> Numbers.Q.t
  val choose : t -> Numbers.Q.t * r
  val subst : r -> t -> t -> t
  val remove : r -> t -> t
  val to_list : t -> (Numbers.Q.t * r) list * Numbers.Q.t
    
  val print : Format.formatter -> t -> unit
  val type_info : t -> Ty.t
  val is_monomial : t -> (Numbers.Q.t * r * Numbers.Q.t) option

  (* PPMC des denominateurs des coefficients excepte la constante *)
  val ppmc_denominators : t -> Numbers.Q.t
  (* PGCD des numerateurs des coefficients excepte la constante *)
  val pgcd_numerators : t -> Numbers.Q.t
  (* retourne un polynome sans constante et sa constante 
     et la constante multiplicative:
     normal_form p = (p',c,d) <=> p = (p' + c) * d *)
  val normal_form : t -> t * Numbers.Q.t * Numbers.Q.t
  (* comme normal_form mais le signe est aussi normalise *)
  val normal_form_pos : t -> t * Numbers.Q.t * Numbers.Q.t

  val abstract_selectors : t -> (r * r) list -> t * (r * r) list
end

module Make (X : S) : T with type r = X.r
  
