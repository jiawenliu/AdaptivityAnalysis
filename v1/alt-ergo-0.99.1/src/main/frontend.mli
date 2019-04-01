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

open Why_ptree

module Time : sig

  val start : unit -> unit
  val get : unit -> float

  val set_timeout : unit -> unit
  val unset_timeout : unit -> unit

end

module type S = sig

  type sat_env

  type output = Unsat of Explanation.t | Inconsistent 
	        | Sat of sat_env | Unknown of sat_env

  val process_decl:
    (Why_ptree.sat_tdecl -> output -> int64 -> 'a) ->
    sat_env * bool * Explanation.t -> sat_tdecl ->
    sat_env * bool * Explanation.t

  val open_file: 
    Lexing.lexbuf -> in_channel ->
    ((int tdecl, int) annoted * Why_typing.env) list list * Smt_ast.status

  val print_status : sat_tdecl -> output -> int64 -> unit
end

module Make (SAT: Sat_solvers.S) : S with type sat_env = SAT.t
