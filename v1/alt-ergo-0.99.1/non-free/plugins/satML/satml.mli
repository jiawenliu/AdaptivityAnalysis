(******************************************************************************)
(*                               OCamlPro                                     *)
(*                                                                            *)
(* Copyright 2013-2014 OCamlPro                                               *)
(* All rights reserved. See accompanying files for the terms under            *)
(* which this file is distributed. In doubt, contact us at                    *)
(* contact@ocamlpro.com (http://www.ocamlpro.com/)                            *)
(*                                                                            *)
(******************************************************************************)

(*==============================================================================

  Acknowledgement: some parts of this file are taken from the release 0.3 of
  Alt-Ergo-Zero (extracted from Cubicle), which is under the terms of the
  Apache Software License version 2.0 (see Copyright below)

(**************************************************************************)
(*                                                                        *)
(*                                  Cubicle                               *)
(*             Combining model checking algorithms and SMT solvers        *)
(*                                                                        *)
(*                  Sylvain Conchon and Alain Mebsout                     *)
(*                  Universite Paris-Sud 11                               *)
(*                                                                        *)
(*  Copyright 2011. This file is distributed under the terms of the       *)
(*  Apache Software License version 2.0                                   *)
(*                                                                        *)
(**************************************************************************)

==============================================================================*)

module Types : sig

  type atom
  type clause 

  val pr_atom : Format.formatter -> atom -> unit
  val pr_clause : Format.formatter -> clause -> unit

  val literal : atom -> Literal.LT.t
  val weight : atom -> float
  val is_true : atom -> bool
  val level : atom -> int
  val index : atom -> int
  val cmp_atom : atom -> atom -> int

(*
  type var
  type reason
  type premise

(*module Make (Dummy : sig end) : sig*)
  
  val neg : atom -> atom

  val cpt_mk_var : int ref
  val ma : var Literal.LT.Map.t ref

  val dummy_var : var
  val dummy_atom : atom
  val dummy_clause : clause

  val make_var : Literal.LT.t -> var * bool

  val add_atom : Literal.LT.t -> atom 
  val vrai_atom  : atom
  val faux_atom  : atom

  val make_clause : string -> atom list -> Formula.t -> int -> bool -> 
  premise-> clause 

  val fresh_name : unit -> string

  val fresh_lname : unit -> string

  val fresh_dname : unit -> string

  val to_float : int -> float

  val to_int : float -> int
  val made_vars_info : unit -> int * var list
  val clear : unit -> unit

(****)

  val eq_atom   : atom -> atom -> bool
  val hash_atom  : atom -> int
  val tag_atom   : atom -> int

  val cmp_var : var -> var -> int
  val eq_var   : var -> var -> bool
  val h_var    : var -> int
  val tag_var  : var -> int

(*end*)

*)
end


(******************************************************************************)

module Flat_Formula : sig

  type t
  type view = private UNIT of Types.atom | AND of t list | OR of t list

  val print   : Format.formatter -> t -> unit

  val view    : t -> view
  val vrai    : t
  val faux    : t

  val mk_lit  : Literal.LT.t -> t
  val mk_not  : t -> t
  val mk_and  : t list -> t
  val mk_or   : t list -> t

  val compare : t -> t -> int
  val equal   : t -> t -> bool

  val simplify :
    Formula.t -> 
    (Formula.t -> t * 'a) -> 
    t * (Formula.t * (t * Types.atom)) list

  val cnf : t ->
    Types.atom list list * Types.atom list list * Types.atom list

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

exception Sat
exception Unsat of Types.clause list

module type SAT_ML = sig

  (*module Make (Dummy : sig end) : sig*)
  type state
  type th

  val solve : unit -> unit
  val assume : Types.atom list list -> Formula.t -> cnumber : int -> unit

  val boolean_model : unit -> Types.atom list
  val current_tbox : unit -> th
  val empty : unit -> unit
  val clear : unit -> unit

  val save : unit -> state
  val restore : state -> unit

  val start : unit -> unit
  val stop : unit -> int64

(*end*)
end

module Make (Th : Theory.S) : SAT_ML with type th = Th.t

