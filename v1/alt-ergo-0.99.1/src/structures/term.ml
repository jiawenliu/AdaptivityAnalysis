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

open Format
open Hashcons

module Sy = Symbols

type view = {f: Sy.t ; xs: t list; ty: Ty.t; depth: int; tag: int;}

and t = view

module Subst = struct
  include Symbols.Map 

  let print pr_elt fmt sbt =
    iter (fun k v -> fprintf fmt "%a -> %a  " Sy.print k pr_elt v) sbt
end

type subst = t Subst.t * Ty.subst
    
module H = struct
  type t = view
  let equal t1 t2 = try
                      Sy.equal t1.f t2.f 
                      && List.for_all2 (==) t1.xs t2.xs 
                      && Ty.equal t1.ty t2.ty
    with Invalid_argument _ -> false
      
  let hash t =
    abs (List.fold_left 
	   (fun acc x-> acc*19 +x.tag) (Sy.hash t.f + Ty.hash t.ty) 
	   t.xs)
  let tag tag x = {x with tag = tag}
end

module T = Make(H)
  
let view t = t

let rec print_silent fmt t = 
  let {f=x;xs=l;ty=ty} = view t in
  match x, l with
    | Sy.Op Sy.Get, [e1; e2] ->
      fprintf fmt "%a[%a]" print e1 print e2

    | Sy.Op Sy.Set, [e1; e2; e3] ->
      fprintf fmt "%a[%a<-%a]" print e1 print e2 print e3

    | Sy.Op Sy.Concat, [e1; e2] ->
      fprintf fmt "%a@@%a" print e1 print e2

    | Sy.Op Sy.Extract, [e1; e2; e3] ->
      fprintf fmt "%a^{%a,%a}" print e1 print e2 print e3

    | Sy.Op (Sy.Access field), [e] ->
      fprintf fmt "%a.%s" print e (Hstring.view field)

    | Sy.Op (Sy.Record), _ ->
      begin match ty with
	| Ty.Trecord {Ty.lbs=lbs} ->
	  assert (List.length l = List.length lbs);
	  fprintf fmt "{";
	  ignore (List.fold_left2 (fun first (field,_) e -> 
	    fprintf fmt "%s%s = %a"  (if first then "" else "; ")
	      (Hstring.view field) print e;
	    false
	  ) true lbs l);
	  fprintf fmt "}";
	| _ -> assert false
      end

    | Sy.Op op, [e1; e2] -> 
      fprintf fmt "(%a %a %a)" print e1 Sy.print x print e2

    | _, [] -> 
      fprintf fmt "%a" Sy.print x
        
    | _, _ ->
      fprintf fmt "%a(%a)" Sy.print x print_list l

and print_verbose fmt t = 
  fprintf fmt "(%a : %a)" print_silent t Ty.print (view t).ty

and print fmt t = 
  if Options.debug () then print_verbose fmt t 
  else print_silent fmt t
    
and print_list_sep sep fmt = function
  | [] -> ()
  | [t] -> print fmt t
  | t::l -> Format.fprintf fmt "%a%s%a" print t sep (print_list_sep sep) l

and print_list fmt = print_list_sep "," fmt


(* fresh variables must be smaller than problem's variables.
   thus, Instead of comparinf t1.tag with t2.tag, 
   we compare t2.tag and t1.tag
   But we keep true and false as repr
*)
let compare t1 t2 =
  let c = Pervasives.compare t2.tag t1.tag in
  if c = 0 then c else
    match (view t1).f, (view t2).f with
      | (Sy.True | Sy.False ), (Sy.True | Sy.False) -> c
      | (Sy.True | Sy.False ), _ -> -1
      | _, (Sy.True | Sy.False ) -> 1
      | _,_ -> c

let sort = List.sort compare

let make s l ty = 
  let d = 1 + List.fold_left (fun z t -> max z t.depth) 0 l in 
  T.hashcons {f=s; xs=l; ty=ty; depth=d; tag=0 (* dumb_value *)}
    
let fresh_name ty = make (Sy.name (Hstring.fresh_string())) [] ty

let is_fresh t = 
  match view t with
    | {f=Sy.Name(hs,_);xs=[]} -> Hstring.is_fresh_string (Hstring.view hs)
    | _ -> false

let shorten t = 
  let {f=f;xs=xs;ty=ty} = view t in
  make f xs (Ty.shorten ty)

let vrai = make (Sy.True) [] Ty.Tbool
let faux = make (Sy.False) [] Ty.Tbool
let void = make (Sy.Void) [] Ty.Tunit

let int i = make (Sy.int i) [] Ty.Tint
let real r = make (Sy.real r) [] Ty.Treal
let bitv bt ty = make (Sy.Bitv bt) [] ty


let is_int t = (view t).ty= Ty.Tint
let is_real t = (view t).ty= Ty.Treal
  
let equal t1 t2 =  t1 == t2
  
let hash t = t.tag
  
let pred t = make (Sy.Op Sy.Minus) [t;int "1"] Ty.Tint
  
let dummy = make Sy.dummy [] Ty.Tint
(* verifier que ce type est correct et voir si on ne peut pas
   supprimer ce dummy*)

module Set = 
  Set.Make(struct type t' = t type t=t' let compare=compare end)
    
module Map = 
  Map.Make(struct type t' = t type t=t' let compare=compare end)
    
let vars_of = 
  let rec vars_of s t = 
    match view t with
	{ f=(Sy.Var _ as v);xs=[]} -> Sy.Set.add v s
      | {xs=l} -> List.fold_left vars_of s l
  in fun t -> vars_of Sy.Set.empty t

let vars_of_as_term = 
  let rec vars_of_as_term s t = 
    match view t with
	{ f=(Sy.Var _ );xs=[]} -> Set.add t s
      | {xs=l} -> List.fold_left vars_of_as_term s l
  in fun t -> vars_of_as_term Set.empty t

    
let vty_of t = 
  let rec vty_of s t = 
    let {xs = xs; ty = ty} = view t in 
    List.fold_left vty_of (Ty.Svty.union s (Ty.vty_of ty)) xs
  in
  vty_of Ty.Svty.empty t

module Hsko = Hashtbl.Make(H)
let gen_sko ty = make (Sy.fresh "@sko") [] ty

let is_skolem_cst v = 
  try
    String.sub (Sy.to_string v.f) 0 4 = "_sko"
  with Invalid_argument _ -> false

let find_skolem = 
  let hsko = Hsko.create 17 in
  fun v ty ->
    if is_skolem_cst v then
      try Hsko.find hsko v
      with Not_found -> 
	let c = gen_sko ty in Hsko.add hsko v c; c
    else v

let rec apply_subst ((s_t,s_ty) as s) t = 
  let {f=f;xs=xs;ty=ty} = view t in
  try 
    let v = Sy.Map.find f s_t in
    find_skolem v ty
  with Not_found -> 
    make f (List.map (apply_subst s) xs) (Ty.apply_subst s_ty ty)

let compare_subst (s_t1, s_ty1) (s_t2, s_ty2) = 
  let c = Ty.compare_subst s_ty1 s_ty2 in
  if c<>0 then c else Sy.Map.compare compare s_t1 s_t2

let fold_subst_term f (s,_) acc = Sy.Map.fold f s acc

let union_subst (s_t1, s_ty1) ((s_t2, s_ty2) as subst) = 
  let s_t = 
    Sy.Map.fold 
      (fun k x s2 -> Sy.Map.add k x s2)
      (Sy.Map.map (apply_subst subst) s_t1) s_t2
  in
  let s_ty = Ty.union_subst s_ty1 s_ty2 in
  s_t, s_ty

let rec subterms acc t = 
  let {xs=xs} = view t in List.fold_left subterms (Set.add t acc) xs


module Labels = Hashtbl.Make(H)
  
let labels = Labels.create 100007
  
let add_label lbl t = 
  Labels.replace labels t lbl
    
let label t = try Labels.find labels t with Not_found -> Hstring.empty


let label_model h =
  try String.sub (Hstring.view h) 0 6 = "model:"
  with Invalid_argument _ -> false

let rec is_in_model_rec depth { f = f; xs = xs } =
  let lb = Symbols.label f in
  (label_model lb
   &&
     (try
	let md = Scanf.sscanf (Hstring.view lb) "model:%d" (fun x -> x) in
	depth <= md
      with Scanf.Scan_failure _ | End_of_file-> true))
  || 
    List.exists (is_in_model_rec (depth +1)) xs

let is_in_model t =
  label_model (label t) || is_in_model_rec 0 t


let is_labeled t = not (Hstring.equal (label t) Hstring.empty)

let print_tagged_classes fmt =
  List.iter (fun cl -> 
    let cl = List.filter is_labeled (Set.elements cl) in
    if cl <> [] then
      fprintf fmt "\n{ %a }" (print_list_sep " , ") cl)

let type_info t = t.ty
let top () = vrai
let bot () = faux
