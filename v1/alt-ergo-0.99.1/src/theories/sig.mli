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

type answer = Yes of Explanation.t * Term.Set.t list | No

type 'a ac = {h: Symbols.t ; t: Ty.t ; l: ('a * int) list}

type 'a literal = LTerm of Literal.LT.t | LSem of 'a Literal.view 

type 'a input =  'a Literal.view * Literal.LT.t option * Explanation.t

type 'a result = { 
  assume : ('a literal * Explanation.t) list;  
  remove: ('a literal * Explanation.t) list;
}

type 'a solve_pb = { sbt : ('a * 'a) list; eqs : ('a * 'a) list }

module type RELATION = sig
  type t
  type r
  type uf
  val empty : Term.Set.t list -> t
    
  val assume : t -> uf -> (r input) list -> t * r result
  val query  : t -> uf -> r input -> answer

  val case_split : t -> (r Literal.view * Explanation.t * Numbers.Q.t) list
  (** case_split env returns a list of equalities *)
    
  val add : t -> r -> t
  (** add a representant to take into account *)

  val print_model : Format.formatter -> t -> (Term.t * r) list -> unit
    
  val new_terms : t -> Term.Set.t
end

module type SHOSTAK = sig

  (**Type of terms of the theory*)
  type t
  (**Type of representants of terms of the theory*)
  type r
  (** Name of the theory*)
  val name : string
  (** return true if the symbol is owned by the theory*)
  val is_mine_symb : Symbols.t -> bool

  (** Give a representant of a term of the theory*)
  val make : Term.t -> r * Literal.LT.t list

  val term_extract : r -> Term.t option * bool (* original term ? *)

  val color : (r ac) -> r
    
  val type_info : t -> Ty.t
    
  val embed : r -> t

  (** Give the leaves of a term of the theory *)
  val leaves : t -> r list
  val subst : r -> r -> t -> r

  val compare : r -> r -> int

  val hash : t -> int
  (** solve r1 r2, solve the equality r1=r2 and return the substitution *)

  val solve : r -> r ->  r solve_pb -> r solve_pb

  val print : Format.formatter -> t -> unit

  val fully_interpreted : Symbols.t -> bool

  val abstract_selectors : t -> (r * r) list -> r * (r * r) list

end

module type X = sig
  type r

  val make : Term.t -> r * Literal.LT.t list
    
  val type_info : r -> Ty.t
    
  val compare : r -> r -> int
    
  val equal : r -> r -> bool

  val hash : r -> int
    
  val leaves : r -> r list
    
  val subst : r -> r -> r -> r
    
  val solve : r -> r ->  (r * r) list
    
  val term_embed : Term.t -> r

  val term_extract : r -> Term.t option * bool (* original term ? *)

  val ac_embed : r ac -> r
    
  val ac_extract : r -> (r ac) option
    
  val color : (r ac) -> r

  val fully_interpreted : Symbols.t -> bool
    
  val print : Format.formatter -> r -> unit
    
  val abstract_selectors : r -> (r * r) list -> r * (r * r) list
    
  val top : unit -> r
  val bot : unit -> r
end
