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

type operator = 
  | Plus | Minus | Mult | Div | Modulo | Concat | Extract 
  | Get | Set | Access of Hstring.t | Record

type name_kind = Ac | Constructor | Other

type t = 
  | True 
  | False
  | Void
  | Name of Hstring.t * name_kind
  | Int of Hstring.t
  | Real of Hstring.t
  | Bitv of string
  | Op of operator
  | Var of Hstring.t

val name : ?kind:name_kind -> string -> t
val var : string -> t
val underscoring : t -> t
val int : string -> t
val real : string -> t

val is_ac : t -> bool

val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

val to_string : t -> string
val print : Format.formatter -> t -> unit

val dummy : t

val fresh : string -> t
  
val is_get : t -> bool 
val is_set : t -> bool 

val fake_eq  : t
val fake_neq : t
val fake_lt  : t
val fake_le  : t

module Map : Map.S with type key = t
module Set : Set.S with type elt = t

val add_label : Hstring.t -> t -> unit
val label : t -> Hstring.t
