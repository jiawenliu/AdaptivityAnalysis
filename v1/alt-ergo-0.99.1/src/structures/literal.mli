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

type 'a view = private
               | Eq of 'a * 'a 
               | Distinct of bool * 'a list
               | Builtin of bool * Hstring.t * 'a list
               | Pred of 'a * bool

type 'a atom_view = 
  | EQ of 'a * 'a
  | BT of Hstring.t * 'a list
  | PR of 'a

module type OrderedType = sig
  type t
  val compare : t -> t -> int
  val hash :  t -> int
  val print : Format.formatter -> t -> unit
  val top : unit -> t
  val bot : unit -> t
  val type_info : t -> Ty.t
end

module type S = sig
  type elt
  type t

  val make : elt view -> t
  val view : t -> elt view
  val atom_view : t -> elt atom_view * bool (* is_negated ? *)

  val mk_eq : elt -> elt -> t
  val mk_distinct : bool -> elt list -> t
  val mk_builtin : bool -> Hstring.t -> elt list -> t
  val mk_pred : elt -> bool -> t

  val mkv_eq : elt -> elt -> elt view
  val mkv_distinct : bool -> elt list -> elt view
  val mkv_builtin : bool -> Hstring.t -> elt list -> elt view
  val mkv_pred : elt -> bool -> elt view

  val neg : t -> t

  val add_label : Hstring.t -> t -> unit
  val label : t -> Hstring.t

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  module Map : Map.S with type key = t
  module Set : Set.S with type elt = t

end

module Make ( X : OrderedType ) : S with type elt = X.t

module type S_Term = sig

  include S with type elt = Term.t

  val vrai : t
  val faux : t

  val apply_subst : Term.subst -> t -> t

  val terms_of : t -> Term.Set.t
  val vars_of : t -> Symbols.Set.t
  val is_in_model : t -> bool

end

module LT : S_Term
