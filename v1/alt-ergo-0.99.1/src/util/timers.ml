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

open Unix
open Format

type kind =
  | TNone
  | TSat
  | TMatch
  | TCC
  | TArith
  | TArrays
  | TSum
  | TRecords
  | TAc

let print fmt = function 
  | TNone -> fprintf fmt "TNone"
  | TSat -> fprintf fmt "TSat"
  | TMatch -> fprintf fmt "TMatch"
  | TCC -> fprintf fmt "TCC"
  | TArith -> fprintf fmt "TArith"
  | TArrays -> fprintf fmt "TArrays"
  | TSum -> fprintf fmt "TSum"
  | TRecords -> fprintf fmt "TRecords"
  | TAc -> fprintf fmt "TAc"

type t = {
  mutable cur_u : float;
  mutable cur_t : kind;
  mutable stack : kind list;
  h:(kind, float ref) Hashtbl.t;
}

let init () =
  let h = Hashtbl.create 8 in
  Hashtbl.add h TSat (ref 0.0);
  Hashtbl.add h TMatch (ref 0.0);
  Hashtbl.add h TCC (ref 0.0);
  Hashtbl.add h TArith (ref 0.0);
  Hashtbl.add h TArrays (ref 0.0);
  Hashtbl.add h TSum (ref 0.0);
  Hashtbl.add h TRecords (ref 0.0);
  Hashtbl.add h TAc (ref 0.0);
  {
    h = h;
    cur_t = TNone;
    cur_u = 0.0;
    stack = [];
  }

let reset h =
  Hashtbl.iter (fun _ cpt ->  cpt := 0.0) h.h;
  h.cur_t <- TNone;
  h.cur_u <- 0.0;
  h.stack <- []


let pause_all _ = assert false


let start h t =
  let cur = (times()).tms_utime in
  begin
    match h.cur_t with
      | TNone -> ()
      | x ->
	let cpt = Hashtbl.find h.h x in
	cpt := !cpt +. (cur -. h.cur_u);
	h.stack <- x::h.stack
  end;
  h.cur_t <- t;
  h.cur_u <- cur


let pause h t =
  let cur = (times()).tms_utime in
  (* assert (h.cur_t = t); *)
  let cpt = Hashtbl.find h.h t in
  cpt := !cpt +. (cur -. h.cur_u);
  h.cur_u <- cur;
  match h.stack with
    | [] -> h.cur_t <- TNone
    | x::st ->
      h.cur_t <- x;
      h.stack <- st



let update h =
  let cur = (times()).tms_utime in
  let t = h.cur_t in
  if t <> TNone then
    let cpt = Hashtbl.find h.h t in
    cpt := !cpt +. (cur -. h.cur_u);
    h.cur_u <- cur


let pause_and_restart h t f = assert false

let get h t = !(Hashtbl.find h.h t)
