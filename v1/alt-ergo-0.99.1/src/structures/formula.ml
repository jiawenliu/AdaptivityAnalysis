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
open Options

module T = Term
module Sy = Symbols

type lemma = {
  qvars: Sy.Set.t;
  triggers : (T.t list * Literal.LT.t option) list;
  main : t;
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
    Unit of t*t
  | Clause of t*t  
  | Literal of Literal.LT.t
  | Lemma of lemma
  | Skolem of skolem
  | Let of llet

and iview = { pos : view ; neg : view ; size : int; tag : int}

and t = iview * int
    
type gformula = { 
  f: t; 
  age: int; 
  lem: t option; 
  from_terms : Term.t list;
  mf: bool;
  gf: bool;
}

module View = struct
  type t = iview
      
  let rec compare_list compare l1 l2 = match l1 , l2 with
      [] , [] -> 0
    | [] , _ -> 1
    | _ , [] -> -1
    | x1::l1 , x2::l2 -> 
      let c = compare x1 x2 in 
      if c<>0 then c else compare_list compare l1 l2
	
  let rec compare_pclause v1 v2 = match v1 , v2 with
    | Unit(x1,y1) , Unit(x2,y2) -> 
      let c = compare_t x1 x2 in if c<>0 then c else compare_t y1 y2
    | Unit _ , _ -> -1
    | _, Unit _ -> 1
    | Clause(x1,y1) , Clause(x2,y2) -> 
      let c = compare_t x1 x2 in if c<>0 then c else compare_t y1 y2
    | Clause _ , _ -> -1
    | _ , Clause _ -> 1
    | Literal a1 , Literal a2 -> Literal.LT.compare a1 a2
    | Literal _ , _ -> -1
    | _ , Literal _ -> 1
    | Lemma l1 , Lemma l2 ->  compare_lemme l1 l2
    | Lemma _ , _ -> -1
    | _ , Lemma _ -> 1
    | Let l1, Let l2 -> compare_let l1 l2
    | Let _, _ -> -1
    | _, Let _ -> 1
    | Skolem s1 , Skolem s2 -> compare_skolem s1 s2
      
  and compare_t (t1,_) (t2,_)  = Pervasives.compare t1.tag t2.tag
  and compare_view v1 v2 = 
    let c = compare_pclause v1.pos v2.pos in
    if c<>0 then c else compare_pclause v1.neg v2.neg
  and compare_lemme l1 l2 = 
    let c = compare_t l1.main l2.main in
    if c<>0 then c else
      let c = Sy.Set.compare l1.qvars l2.qvars in
      if c<>0 then c else 
	compare_list (fun (t1,_) (t2,_) -> compare_list T.compare t1 t2)
	  l1.triggers l2.triggers
  and compare_skolem s1 s2 = 
    let c = compare_t s1.sko_f s2.sko_f in
    if c<>0 then c else
      (*      let c = Ty.compare_subst s1.ssubst_ty s2.ssubst_ty in
              if c<>0 then c else Sy.Map.compare T.compare s1.ssubst s2.ssubst*)
      Term.compare_subst s1.sko_subst s2.sko_subst
  and compare_let l1 l2 =
    let c = compare_t l1.let_f l2.let_f in
    if c<>0 then c else 
      let c = T.compare l1.let_term l2.let_term in
      if c<>0 then c else 
        let c = Sy.compare l1.let_var l2.let_var in
        if c<>0 then c else
	  T.compare_subst l1.let_subst l2.let_subst
  (*let c = Ty.compare_subst l1.lsubst_ty l2.lsubst_ty in
    if c<>0 then c else
    Sy.Map.compare T.compare l1.lsubst l2.lsubst*)
            
  let sort l = 
    let l = List.sort compare_t l in
    List.fold_left 
      (fun acc x -> match acc with
	  [] -> [x]
	| h::l when fst x == fst h -> acc
	| _ -> x::acc) [] l
      
  let eqc c1 c2 = match c1,c2 with
    | Unit((f1, _), (f2, _)) , Unit((g1,_), (g2,_)) ->
      f1==g1 && f2==g2 || f1==g2 && f2==g1

    | Clause((f1, _), (f2, _)) , Clause((g1,_), (g2,_)) ->
      f1==g1 && f2==g2 || f1==g2 && f2==g1

    | Literal x , Literal y -> Literal.LT.equal x y

    | Lemma({triggers = lt1; main = (f1,_)}),
      Lemma({triggers = lt2; main = (f2,_)}) -> 
      (try 
	 List.for_all2 
	   (fun (l1, _) (l2, _) -> List.for_all2 T.equal l1 l2) lt1 lt2 && 
	   f1==f2
       with Invalid_argument _ -> false)

    | Skolem {sko_subst = s1; sko_f = (f1, _)}, 
	Skolem {sko_subst = s2; sko_f = (f2,_)} -> 
      f1==f2  && Term.compare_subst s1 s2 = 0

    (*
      | Skolem {ssubst=s1;ssubst_ty=ty1;sf=(f1,_)},
      Skolem {ssubst=s2;ssubst_ty=ty2;sf=(f2,_)} -> 
      f1==f2 
      && (Sy.Map.compare T.compare s1 s2 = 0)
      && Ty.compare_subst ty1 ty2 = 0
    *)
    | Let l1, Let l2 -> 
      fst l1.let_f == fst l2.let_f 
      && Sy.equal l1.let_var l2.let_var 
      && Term.equal l1.let_term l2.let_term 
      && Term.compare_subst l1.let_subst l2.let_subst = 0
    (*&& (Sy.Map.compare T.compare l1.lsubst l2.lsubst = 0)
      && Ty.compare_subst l1.lsubst_ty l2.lsubst_ty = 0*)
	  
    | _, _ -> false
      
  let hashlt = List.fold_left (fun acc x->acc*19 + T.hash x)
  let hashllt = List.fold_left (fun acc (x, _) ->acc*19 + hashlt 0 x)
    
  let hashc acc = function 
    | Unit((f1,_),(f2,_)) -> (* XXX : Same as Clause ? *)
      let min = min f1.tag f2.tag in
      let max = max f1.tag f2.tag in
      (acc*19 + min)*19 + max
    | Clause((f1,_),(f2,_)) -> 
      let min = min f1.tag f2.tag in
      let max = max f1.tag f2.tag in
      (acc*19 + min)*19 + max
    | Lemma({qvars = vars;triggers = trs; main = (f,_)}) -> 
      hashllt (Hashtbl.hash (f.tag,vars)) trs
    | Literal x -> Literal.LT.hash x

    | Skolem{sko_subst = s; sko_f = (f,_)} -> 
      T.fold_subst_term
	(fun s t acc -> acc * 19 + Sy.hash s + T.hash t) s f.tag

    (*    | Skolem{ssubst=s;sf=(f,_)} -> 
	  Sy.Map.fold 
	  (fun s t acc ->acc * 19 + Sy.hash s) s f.tag*)

    | Let ({let_var=lvar; let_term=lterm; 
	    let_subst=s; let_f=(lf,_)}) -> 
      T.fold_subst_term 
	(fun s t acc ->acc * 19 + Sy.hash s) s
	(lf.tag * 19 * 19 + Sy.hash lvar * 19 + acc)

  (*        Sy.Map.fold (fun s t acc ->acc * 19 + Sy.hash s) lsubst 
            (lf.tag * 19 * 19 + Sy.hash lvar * 19 + acc)*)

  (*    | Let ({let_var=lvar; let_term=lterm;lsubst=lsubst;let_f=(lf,_)}) -> 
        Sy.Map.fold (fun s t acc ->acc * 19 + Sy.hash s) lsubst 
        (lf.tag * 19 * 19 + Sy.hash lvar * 19 + acc)*)
	
  let equal f1 f2 = eqc f1.pos f2.pos && eqc f1.neg f2.neg
  let hash f = abs (hashc (hashc 1 f.pos) f.neg)
  let tag tag x = {x with tag = tag}
end
  
  
module H = Make(View)
  
let iview f = f

let view (t,_) = t.pos
let id (_,id) = id

let rec print fmt f = 
  match view f with
    | Literal a -> 
      Literal.LT.print fmt a
    | Lemma {triggers = trs; main = f; name = n} -> 
      if verbose () then
	let first = ref true in
	fprintf fmt "(lemma: %s)[%a]@  %a" 
	  n
	  (fun fmt -> 
	    List.iter (fun (l, _) -> 
	      fprintf fmt "%s%a"
		(if !first then "" else " | ") T.print_list l;
	      first := false;
	    ))
	  trs print f
      else 
	fprintf fmt "lem %s" n

    | Unit(f1, f2) -> fprintf fmt "@[(%a /\\@ %a)@]" print f1 print f2

    | Clause(f1, f2) -> fprintf fmt "@[(%a \\/@ %a)@]" print f1 print f2

    | Skolem{sko_f=f} -> fprintf fmt "<sko> (%a)" print f

    | Let l -> 
      fprintf fmt 
	"let %a =@ %a in@ %a" Sy.print l.let_var 
	Term.print l.let_term print l.let_f

(* let print fmt ((_,id) as f) = *)
(*   fprintf fmt "(%d)%a" id print f *)


let union_subst s1 ((s2,s2_ty) as subst) = 
  Sy.Map.fold 
    (fun k x s2 -> Sy.Map.add k x s2) (Sy.Map.map (T.apply_subst subst)  s1) s2


let size (t,_) = t.size

let compare ((v1,_) as f1) ((v2,_) as f2)= 
  let c = Pervasives.compare (size f1) (size f2) in 
  if c=0 then compare v1.tag v2.tag else c
    
let equal (f1,_) (f2,_) = f1 == f2

let hash (f, _) = f.tag


(* smart constructors *)
let make pos neg size id =
  (H.hashcons {pos = pos; neg = neg; size = size; tag = -1 (* dumb *)}, id)
    
let mk_not (f,id) =
  let f = iview f in
  make f.neg f.pos f.size id

let vrai = make (Literal Literal.LT.vrai) (Literal Literal.LT.faux) 1 0
let faux = mk_not vrai

let mk_skolem_subst bv v = 
  T.Set.fold 
    (fun x m -> 
      let {T.f=x;ty=ty} = T.view x in
      let bv = T.Set.fold (fun y acc-> y::acc) bv [] in
      let t = T.make (Sy.fresh "_sko") bv ty in
      Sy.Map.add x t m) 
    v Sy.Map.empty

let symbols_of_terms v = 
  T.Set.fold 
    (fun t sy -> let {T.f=f} = T.view t in Sy.Set.add f sy) 
    v Sy.Set.empty
    
(* name: (forall bv [trs]. f[fv]) *)
let mk_forall up bv trs f name id = 
  let sy = symbols_of_terms bv in
  let lem = {qvars = sy; triggers = trs; main = f ; name=name} in
  (*  let sko = {ssubst = mk_skolem_subst up bv;
      ssubst_ty = Ty.esubst;
      sf = mk_not f} in*)
  let sko = 
    {sko_subst = (mk_skolem_subst up bv, Ty.esubst); sko_f = mk_not f} 
  in
  make (Lemma(lem)) (Skolem(sko)) (size f) id
    
(* forall upbv.  name: (exists bv [trs]. f) *)
let mk_exists up bv trs f name id= 
  let sy = symbols_of_terms bv in
  let lem = 
    {qvars = sy; triggers = trs; main = mk_not f; name=name} 
  in
  let sko = {sko_subst = (mk_skolem_subst up bv, Ty.esubst); sko_f = f} in
  make (Skolem(sko)) (Lemma(lem)) (size f) id

(* forall up. let bv = t in f *)
let mk_let _up bv t f id =
  let {Term.ty=ty} = Term.view t in
  let up = Term.vars_of_as_term t in
  let up = T.Set.fold (fun y acc-> y::acc) up [] in
  let subst = Sy.Map.add bv (T.make (Sy.fresh "_let") up ty) Sy.Map.empty in
  make
    (Let{let_var=bv; let_subst=(subst, Ty.esubst); let_term=t; let_f=f})
    (Let{let_var=bv; let_subst=(subst, Ty.esubst); let_term=t; let_f=mk_not f})
    (size f) id
    
let mk_and f1 f2 id =
  if equal f1 (mk_not f2) then faux
  else
    if equal f1 f2 then f1
    else if equal f1 vrai then f2
    else if equal f2 vrai then f1
    else if (equal f1 faux) || (equal f2 faux) then faux
    else
      let f1, f2 = if compare f1 f2 < 0 then f1, f2 else f2, f1 in
      let size = size f1 + size f2 in
      make (Unit(f1,f2)) (Clause(mk_not f1,mk_not f2)) size id
        
let mk_or f1 f2 id = 
  if equal f1 (mk_not f2) then vrai
  else
    if equal f1 f2 then f1 
    else if equal f1 faux then f2
    else if equal f2 faux then f1
    else if equal f1 vrai || equal f2 vrai then vrai
    else
      let f1, f2 = if compare f1 f2 < 0 then f1, f2 else f2, f1 in
      let size = size f1 + size f2 in
      make (Clause(f1,f2)) (Unit(mk_not f1,mk_not f2)) size id
        
let mk_imp f1 f2 id = 
  let size = size f1 + size f2 in
  make (Clause(mk_not f1,f2)) (Unit(f1,mk_not f2)) size id

let mk_iff f1 f2 id = 
  let a = mk_or f1 f2 id in
  let b = mk_or (mk_not f1) (mk_not f2) id in
  let c = mk_or (mk_not f1) f2 id in
  let d = mk_or f1 (mk_not f2) id in
  make (Unit(c,d)) (Unit(a,b)) (2*(size f1+size f2)) id

(* let mk_lit a id = make (Literal a) (Literal (Literal.LT.neg a)) 1 id *)

let translate_eq_to_iff s t = 
  (T.view s).T.ty = Ty.Tbool && 
  not 
  (T.equal s T.vrai || T.equal s T.faux || T.equal t T.vrai ||T.equal t T.faux)
  
let mk_lit a id = match Literal.LT.view a with
  | Literal.Eq(s,t) when translate_eq_to_iff s t ->
    let a1 = Literal.LT.mk_pred s false in
    let a2 = Literal.LT.mk_pred t false in
    let f1 = make (Literal a1) (Literal (Literal.LT.neg a1)) 1 id in
    let f2 = make (Literal a2) (Literal (Literal.LT.neg a2)) 1 id in
    mk_iff f1 f2 id

  | Literal.Distinct(false,[s;t]) when translate_eq_to_iff s t ->
    let a1 = Literal.LT.mk_pred s false in
    let a2 = Literal.LT.mk_pred t false in
    let f1 = make (Literal a1) (Literal (Literal.LT.neg a1)) 1 id in
    let f2 = make (Literal a2) (Literal (Literal.LT.neg a2)) 1 id in
    mk_not (mk_iff f1 f2 id)
      
  | _ -> make (Literal a) (Literal (Literal.LT.neg a)) 1 id

let mk_if t f2 f3 id = 
  let lit = mk_lit (Literal.LT.mk_pred t false) id in
  mk_or (mk_and lit f2 id) (mk_and (mk_not lit) f3 id) id

(* this function should only be applied with ground substitutions *)
let rec apply_subst subst (f, id) =
  let {pos=p;neg=n;size=s} = iview f in
  let sp, sn = iapply_subst subst p n in 
  match sp with
    | Literal a      -> mk_lit a id     (* this may simplifies the result *)
    | Unit (f1, f2)  -> mk_and f1 f2 id (* this may simplifies the result *)
    | Clause (f1,f2) -> mk_or f1 f2 id  (* this may simplifies the result *)
    | _              -> make sp sn s id

and iapply_subst ((s_t,s_ty) as subst) p n = match p, n with
  | Literal a, Literal _ ->
    let sa = Literal.LT.apply_subst subst a in
    let nsa = Literal.LT.neg sa in
    Literal(sa), Literal(nsa)

  | Lemma({qvars = vars; triggers = trs; main = f} as lem), Skolem sko
  | Skolem sko, Lemma({qvars = vars; triggers = trs; main = f} as lem)->
    let s_t = Sy.Set.fold Sy.Map.remove vars s_t in
    let subst = s_t , s_ty in
    let f = apply_subst subst f in
    let trs =
      List.map (fun (l, r) -> List.map (T.apply_subst subst) l, r) trs in
    let slem = Lemma({lem with triggers = trs; main = f}) in
    let sigma = T.union_subst sko.sko_subst subst in
    let ssko = Skolem {sko with sko_subst = sigma } in
    (match p,n with
      | Lemma _, Skolem _ -> slem, ssko
      | Skolem _, Lemma _ -> ssko, slem
      | _ -> assert false)

  | Unit(f1, f2), _ ->
    let sf1 = apply_subst subst f1 in
    let sf2 = apply_subst subst f2 in
    Unit(sf1, sf2), Clause(mk_not sf1, mk_not sf2)

  | Clause(f1, f2), _ -> 
    let sf1 = apply_subst subst f1 in
    let sf2 = apply_subst subst f2 in
    Clause(sf1, sf2), Unit(mk_not sf1, mk_not sf2)

  | Let ({let_subst = s; let_term = lterm; let_f = lf} as e), Let _ ->
    let lterm = T.apply_subst subst lterm in
    let se = { e with let_subst = T.union_subst s subst; let_term = lterm } in
    let sne = { se with let_f = mk_not lf } in
    Let se, Let sne

  | _ -> assert false

let add_label lbl f =
  match view f with
    | Literal a -> 
      Literal.LT.add_label lbl a;
      Literal.LT.add_label lbl (Literal.LT.neg a)
    | _ -> ()

let label f = 
  match view f with
    | Literal l -> Literal.LT.label l
    | _ -> Hstring.empty

let label_model h =
  try String.sub (Hstring.view h) 0 6 = "model:"
  with Invalid_argument _ -> false

let is_in_model f =
  match view f with
    | Literal l -> 
      label_model (Literal.LT.label l) || Literal.LT.is_in_model l
    | _ -> false
      
let free_vars =
  let rec free_rec acc f = 
    match view f with
	Literal a -> Sy.Set.union (Literal.LT.vars_of a) acc
      | Lemma {qvars = v; main = f} -> 
	let s = free_rec acc f in Sy.Set.diff s v
      | Unit(f1,f2) -> free_rec (free_rec acc f1) f2
      | Clause(f1,f2) -> free_rec (free_rec acc f1) f2
      | Skolem{sko_subst = (subst,_); sko_f = f} -> 
	let sy = free_rec acc f in
	Sy.Map.fold 
	  (fun s t sy -> 
	    if Sy.Set.mem s sy then 
	      Sy.Set.remove s (Sy.Set.union sy (Term.vars_of t))
	    else sy
	  ) subst sy
      | Let {let_subst = (subst, _); let_term = t; let_f = lf} ->
	let ss =  
	  Sy.Set.filter (fun x -> Sy.Map.mem x subst) (free_rec acc lf) in
        let sy = Sy.Set.union (Term.vars_of t) ss in
        Sy.Map.fold
	  (fun s t sy -> 
	    if Sy.Set.mem s sy then 
	      Sy.Set.remove s (Sy.Set.union sy (Term.vars_of t))
	    else sy
	  ) subst sy

  (*      | Let {lsubst=subst; let_term=t; let_f=lf} ->
	  let ss =  
	  Sy.Set.filter (fun x -> Sy.Map.mem x subst) (free_rec acc lf) in
          let sy = Sy.Set.union (Term.vars_of t) ss in
          Sy.Map.fold
	  (fun s t sy -> 
	  if Sy.Set.mem s sy then 
	  Sy.Set.remove s (Sy.Set.union sy (Term.vars_of t))
	  else sy
	  ) subst sy*)
          
  in free_rec Sy.Set.empty
  
let terms = 
  let rec terms acc f = match view f with
    | Literal a -> 
      let s = 
	T.Set.filter 
	  (fun t-> 
	    Sy.Set.is_empty (T.vars_of t) && Ty.Svty.is_empty (T.vty_of t)
	  ) (Literal.LT.terms_of a)
      in
      T.Set.union s acc
    | Lemma {triggers = trs; main = f} -> terms acc f
    | Unit(f1,f2) -> terms (terms acc f1) f2
    | Clause(f1,f2) -> terms (terms acc f1) f2
    | Skolem{sko_subst = (s,_); sko_f = f} -> terms acc f
    | Let {let_term=t; let_f=lf} -> 
      let st = 
	T.Set.filter 
          (fun t->
            Sy.Set.is_empty (T.vars_of t) && Ty.Svty.is_empty (T.vty_of t)) 
	  (Term.subterms Term.Set.empty t) 
      in
      terms (T.Set.union st acc) lf
  in terms T.Set.empty

module Set = Set.Make(struct type t'=t type t=t' let compare=compare end)
module Map = Map.Make(struct type t'=t type t=t' let compare=compare end)

