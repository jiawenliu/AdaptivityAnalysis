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

type lemma =
    { qvars: Symbols.Set.t;  (* toplevel quantified variables *)
      triggers : (Term.t list * Literal.LT.t option) list; (* multi-triggers *)
      main : t;  (* the main lemma's formula *)
      name : string; 
    }
      
and llet = {
  let_var: Symbols.t;
  let_subst : Term.subst;
  let_term : Term.t;
  let_f : t;
}

and skolem = {
  sko_subst : Term.subst;
  sko_f : t;
}

and view = 
    Unit of t*t  (* unit clauses *)
  | Clause of t*t      (* a clause (t1 or t2) *)
  | Literal of Literal.LT.t   (* an atom *)
  | Lemma of lemma   (* a lemma *)
  | Skolem of skolem  (* lazy substitution *)
  | Let of llet (* a binding of a term *)


type gformula = { 
  f: t; 
  age: int; 
  lem: t option; 
  from_terms : Term.t list;
  mf: bool;
  gf: bool;
}

val mk_not : t -> t
val mk_and : t -> t -> int -> t
val mk_or : t -> t -> int -> t
val mk_imp : t -> t -> int -> t
val mk_if : Term.t -> t -> t -> int -> t
val mk_iff : t -> t -> int -> t
val mk_lit : Literal.LT.t -> int -> t
val mk_forall : Term.Set.t -> Term.Set.t -> 
  (Term.t list * Literal.LT.t option) list -> t -> string -> int -> t
val mk_exists : Term.Set.t -> Term.Set.t -> 
  (Term.t list * Literal.LT.t option) list -> t ->
  string -> int -> t
val mk_let : Term.Set.t -> Symbols.t -> Term.t -> t -> int -> t

val add_label : Hstring.t -> t -> unit
val label : t -> Hstring.t
val is_in_model : t -> bool

val view : t -> view
val size : t -> int
val id : t -> int

val print : Format.formatter -> t -> unit

val terms : t -> Term.Set.t
val free_vars : t -> Symbols.Set.t

val apply_subst : Term.subst -> t -> t 

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int
val vrai : t
val faux : t

module Set : Set.S with type elt = t
module Map : Map.S with type key = t

