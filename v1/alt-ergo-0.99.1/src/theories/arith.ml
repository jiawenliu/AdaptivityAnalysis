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
open Options
open Sig
module A = Literal
module Sy = Symbols
module T = Term

module Z = Numbers.Z
module Q = Numbers.Q

let ale = Hstring.make "<=" 
let alt = Hstring.make "<"
let is_mult h = Sy.equal (Sy.Op Sy.Mult) h
let mod_symb = Sy.name "@mod"


module Type (X:Sig.X) : Polynome.T with type r = X.r = struct
  include
    Polynome.Make(struct
      include X
      module Ac = Ac.Make(X)
      let mult v1 v2 =
        X.ac_embed
          { h = Sy.Op Sy.Mult;
	    t = X.type_info v1;
	    l = let l2 = match X.ac_extract v1 with
	      | Some {h=h; l=l} when Sy.equal h (Sy.Op Sy.Mult) -> l
	      | _ -> [v1, 1]
	        in Ac.add (Sy.Op Sy.Mult) (v2,1) l2 
          }
    end)
end

module type EXTENDED_Polynome = sig
  include Polynome.T
  val extract : r -> t option
  val embed : t -> r
end

module Shostak 
  (X : Sig.X)
  (P : EXTENDED_Polynome with type r = X.r) = struct

    type t = P.t

    type r = P.r
        
    module Ac = Ac.Make(X)

    let name = "arith"

    (*BISECT-IGNORE-BEGIN*)
    module Debug = struct

      let solve_aux r1 r2 =
        if debug_arith () then 
          fprintf fmt "[arith] we solve %a=%a@." X.print r1 X.print r2

      let solve_one r1 r2 sbs = 
        if debug_arith () then
          begin
            fprintf fmt "[arith] solving %a = %a yields:@."
              X.print r1 X.print r2;
            let c = ref 0 in
            List.iter 
              (fun (p,v) -> 
                incr c;
                fprintf fmt " %d) %a |-> %a@." !c X.print p X.print v) sbs
          end
    end
    (*BISECT-IGNORE-END*)

    let is_mine_symb = function
      | Sy.Int _ | Sy.Real _ 
      | Sy.Op (Sy.Plus | Sy.Minus | Sy.Mult | Sy.Div | Sy.Modulo) -> true
      | _ -> false

    let empty_polynome ty = P.create [] Q.zero ty

    let is_mine p = match P.is_monomial p with
      | Some (a,x,b) when Q.equal a Q.one && Q.sign b = 0 -> x
      | _ -> P.embed p
        
    let embed r = match P.extract r with
      | Some p -> p
      | _ -> P.create [Q.one, r] Q.zero (X.type_info r)  

    (* t1 % t2 = md  <-> 
       c1. 0 <= md ;
       c2. md < t2 ;
       c3. exists k. t1 = t2 * k + t ;
       c4. t2 <> 0 (already checked) *)
    let mk_modulo md t1 t2 p2 ctx = 
      let zero = T.int "0" in
      let c1 = A.LT.mk_builtin true ale [zero; md] in
      let c2 =
        match P.is_const p2 with
	  | Some n2 -> 
	    let an2 = Q.abs n2 in
	    assert (Q.is_integer an2);
	    let t2 = T.int (Q.string_of an2) in
	    A.LT.mk_builtin true alt [md; t2]
	  | None -> 
	    A.LT.mk_builtin true alt [md; t2]
      in
      let k  = T.fresh_name Ty.Tint in
      let t3 = T.make (Sy.Op Sy.Mult) [t2;k] Ty.Tint in
      let t3 = T.make (Sy.Op Sy.Plus) [t3;md] Ty.Tint in
      let c3 = A.LT.mk_eq t1 t3 in
      c3 :: c2 :: c1 :: ctx    

    let mk_euc_division p p2 t1 t2 ctx = 
      match P.to_list p2 with
        | [], coef_p2 ->
          let md = T.make (Sy.Op Sy.Modulo) [t1;t2] Ty.Tint in
          let r, ctx' = X.make md in
          let rp =
            P.mult (P.create [] (Q.div Q.one coef_p2) Ty.Tint) (embed r) in 
          P.sub p rp, ctx' @ ctx
        | _ -> assert false

    let rec mke coef p t ctx =
      let {T.f = sb ; xs = xs; ty = ty} = T.view t in
      match sb, xs with
        | (Sy.Int n | Sy.Real n) , _  -> 
	  let c = Q.mult coef (Q.of_string (Hstring.view n)) in
	  P.add (P.create [] c ty) p, ctx

        | Sy.Op Sy.Mult, [t1;t2] ->
	  let p1, ctx = mke coef (empty_polynome ty) t1 ctx in
	  let p2, ctx = mke Q.one (empty_polynome ty) t2 ctx in
	  P.add p (P.mult p1 p2), ctx

        | Sy.Op Sy.Div, [t1;t2] -> 
	  let p1, ctx = mke Q.one (empty_polynome ty) t1 ctx in
	  let p2, ctx = mke Q.one (empty_polynome ty) t2 ctx in
	  let p3, ctx = 
	    try 
              let p, approx = P.div p1 p2 in
              if approx then mk_euc_division p p2 t1 t2 ctx
              else p, ctx
	    with Division_by_zero | Polynome.Maybe_zero -> 
              P.create [Q.one, X.term_embed t] Q.zero ty, ctx
	  in
	  P.add p (P.mult (P.create [] coef ty) p3), ctx
	    
        | Sy.Op Sy.Plus , [t1;t2] -> 
	  let p2, ctx = mke coef p t2 ctx in
	  mke coef p2 t1 ctx

        | Sy.Op Sy.Minus , [t1;t2] -> 
	  let p2, ctx = mke (Q.minus coef) p t2 ctx in
	  mke coef p2 t1 ctx

        | Sy.Op Sy.Modulo , [t1;t2] -> 
	  let p1, ctx = mke Q.one (empty_polynome ty) t1 ctx in
	  let p2, ctx = mke Q.one (empty_polynome ty) t2 ctx in
          let p3, ctx = 
            try P.modulo p1 p2, ctx
            with e ->
	      let t = T.make mod_symb [t1; t2] Ty.Tint in    
              let ctx = match e with
                | Division_by_zero | Polynome.Maybe_zero -> ctx
                | Polynome.Not_a_num -> mk_modulo t t1 t2 p2 ctx
                | _ -> assert false 
              in 
              P.create [Q.one, X.term_embed t] Q.zero ty, ctx 
	  in         
	  P.add p (P.mult (P.create [] coef ty) p3), ctx
	    
        | _ ->
	  let a, ctx' = X.make t in
	  let ctx = ctx' @ ctx in
	  match P.extract a with
	    | Some p' -> P.add p (P.mult (P.create [] coef ty) p'), ctx
	    | _ -> P.add p (P.create [coef, a] Q.zero ty), ctx

    let make t =
      Options.tool_req 4 "TR-Arith-Make";
      let {T.ty = ty} = T.view t in
      let p, ctx = mke Q.one (empty_polynome ty) t [] in
      is_mine p, ctx

    let rec expand p n acc =
      assert (n >=0);
      if n = 0 then acc else expand p (n-1) (p::acc)

    let unsafe_ac_to_arith {h=sy; l=rl; t=ty} =
      let mlt = List.fold_left (fun l (r,n) -> expand (embed r)n l) [] rl in
      List.fold_left P.mult (P.create [] Q.one ty) mlt


    let rec number_of_vars l = 
      List.fold_left (fun acc (r, n) -> acc + n * nb_vars_in_alien r) 0 l 

    and nb_vars_in_alien r = 
      match P.extract r with
        | Some p -> 
	  let l, _ = P.to_list p in
          List.fold_left (fun acc (a, x) -> max acc (nb_vars_in_alien x)) 0 l
        | None -> 
	  begin
	    match X.ac_extract r with
	      | Some ac when is_mult ac.h -> 
		number_of_vars ac.l
	      | _ -> 1
	  end

    let max_list_ = function
      | [] -> 0
      | [ _, x ] -> nb_vars_in_alien x
      | (_, x) :: l ->
	let acc = nb_vars_in_alien x in
	List.fold_left (fun acc (_, x) -> max acc (nb_vars_in_alien x)) acc l

    let contains_a_fresh_alien xp =
      List.exists
        (fun x -> 
          match X.term_extract x with
            | Some t, _ -> Term.is_fresh t 
            | _ -> false
        ) (X.leaves xp)

    let color ac = 
      match ac.l with
        | [(r, 1)] -> assert false
        | _ -> 
          let p = unsafe_ac_to_arith ac in 
          let xp = is_mine p in
          if contains_a_fresh_alien xp then 
	    let l, _ = P.to_list p in
            let mx = max_list_ l in
            if mx = 0 || mx = 1 || number_of_vars ac.l > mx then is_mine p 
	    else X.ac_embed ac
          else xp

    let type_info p = P.type_info p

    module SX = Set.Make(struct type t = r let compare = X.compare end)

    let xs_of_list = 
      List.fold_left (fun s x -> SX.add x s) SX.empty
        
    let rec leaves p = 
      let s = 
        List.fold_left
	  (fun s (_, a) -> SX.union (xs_of_list (X.leaves a)) s)
	  SX.empty (fst (P.to_list p))
      in
      SX.elements s

    let subst x t p = 
      let p = P.subst x (embed t) p in
      let ty = P.type_info p in
      let l, c = P.to_list p in
      let p  = 
        List.fold_left
          (fun p (ai, xi) ->
	    let xi' = X.subst x t xi in
	    let p' = match P.extract xi' with
	      | Some p' -> P.mult (P.create [] ai ty) p'
	      | _ -> P.create [ai, xi'] Q.zero ty
	    in
	    P.add p p')
          (P.create [] c ty) l
      in 
      is_mine p


    let compare_mine = P.compare

    let compare x y = P.compare (embed x) (embed y)

    let hash = P.hash

    (* symmetric modulo p 131 *)
    let mod_sym a b = 
      let m = Q.modulo a b in 
      let m = 
        if Q.sign m < 0 then
          if Q.compare m (Q.minus b) >= 0 then Q.add m b else assert false
        else 
          if Q.compare m b <= 0 then m else assert false
	    
      in
      if Q.compare m (Q.div b (Q.of_int 2)) < 0 then m else Q.sub m b

    let mult_const p c =
      P.mult p (P.create [] c (P.type_info p))
        
    let map_monomes f l ax =
      List.fold_left
        (fun acc (a,x) -> 
          let a = f a in if Q.sign a = 0 then acc else (a, x) :: acc)
        [ax] l 

    let apply_subst sb v = 
      is_mine (List.fold_left (fun v (x, p) -> embed (subst x p v)) v sb)

    (* substituer toutes variables plus grandes que x *)
    let subst_bigger x l = 
      List.fold_left 
        (fun (l, sb) (b, y) ->
          if X.compare y x > 0 then
	    let k = X.term_embed (T.fresh_name Ty.Tint) in
	    (b, k) :: l, (y, embed k)::sb
	  else (b, y) :: l, sb)
        ([], []) l

    let is_mine_p = List.map (fun (x,p) -> x, is_mine p)
      
    let extract_min = function
      | [] -> assert false
      | [c] -> c, []
      | (a, x) :: s -> 
	List.fold_left 
	  (fun ((a, x), l) (b, y) ->
	    if Q.compare (Q.abs a) (Q.abs b) <= 0 then 
	      (a, x), ((b, y) :: l) 
	    else (b, y), ((a, x):: l)) ((a, x),[]) s
          

    (* Decision Procedures. Page 131 *)
    let rec omega l b = 
      
      (* 1. choix d'une variable donc le |coef| est minimal *)
      let (a, x), l = extract_min l in 

      (* 2. substituer les aliens plus grand que x pour 
         assurer l'invariant sur l'ordre AC *)
      let l, sbs = subst_bigger x l in
      let p = P.create l b Ty.Tint in
      assert (Q.sign a <> 0);
      if Q.equal a Q.one then 
        (* 3.1. si a = 1 alors on a une substitution entiere pour x *)
        let p = mult_const p Q.m_one in 
        (x, is_mine p) :: (is_mine_p sbs)
      else if Q.equal a Q.m_one then
        (* 3.2. si a = -1 alors on a une subst entiere pour x*)
        (x,is_mine p) :: (is_mine_p sbs)
      else
        (* 4. sinon, (|a| <> 1) et a <> 0 *)
        (* 4.1. on rend le coef a positif s'il ne l'est pas deja *)
        let a, l, b = 
          if Q.sign a < 0  then 
	    (Q.minus a,
	     List.map (fun (a,x) -> Q.minus a,x) l, (Q.minus b))
          else (a, l, b)
        in
        (* 4.2. on reduit le systeme *)
        omega_sigma sbs a x l b
          
    and omega_sigma sbs a x l b =
      
      (* 1. on definie m qui vaut a + 1 *)
      let m = Q.add a Q.one in

      (* 2. on introduit une variable fraiche *)
      let sigma = X.term_embed (T.fresh_name Ty.Tint) in
      
      (* 3. l'application de la formule (5.63) nous donne la valeur du pivot x*)
      let mm_sigma = (Q.minus m, sigma) in
      let l_mod = map_monomes (fun a -> mod_sym a m) l mm_sigma in

      (* 3.1. Attention au signe de b : 
         on le passe a droite avant de faire mod_sym, d'ou Q.minus *)
      let b_mod = Q.minus (mod_sym (Q.minus b) m) in
      let p = P.create l_mod b_mod Ty.Tint in

      let sbs = (x, p) :: sbs in
      
      (* 4. on substitue x par sa valeur dans l'equation de depart. 
         Voir la formule (5.64) *)
      let p' = P.add (P.mult_const a p) (P.create l b Ty.Tint) in
      
      (* 5. on resoud sur l'equation simplifiee *)
      let sbs2 = solve_int p' in

      (* 6. on normalise sbs par sbs2 *)
      let sbs =  List.map (fun (x, v) -> x, apply_subst sbs2 v) sbs in

      (* 7. on supprime les liaisons inutiles de sbs2 et on merge avec sbs *)
      let sbs2 = List.filter (fun (y, _) -> y <> sigma) sbs2 in
      List.rev_append sbs sbs2

    and solve_int p = 
      if P.is_empty p then raise Not_found;
      let pgcd = P.pgcd_numerators p in
      let ppmc = P.ppmc_denominators p in
      let p = mult_const p (Q.div ppmc pgcd)  in
      let l, b = P.to_list p in
      if not (Q.is_integer b) then raise Exception.Unsolvable;
      omega l b

    let is_null p = 
      if Q.sign (snd (P.to_list p)) <> 0 then raise Exception.Unsolvable; 
      []

    let solve_int p = 
      try solve_int p with Not_found -> is_null p

    let solve_real p =
      try
        let a, x = P.choose p in
        let p = 
	  P.mult 
	    (P.create [] (Q.div Q.m_one a) (P.type_info p))
	    (P.remove x p) 
        in
        [x, is_mine p]
      with Not_found -> is_null p
        

    let unsafe_ac_to_arith {h=sy; l=rl; t=ty} =
      let mlt = List.fold_left (fun l (r, n) -> expand (embed r) n l) [] rl in
      List.fold_left P.mult (P.create [] Q.one ty) mlt

    let safe_distribution p = 
      let l, c = P.to_list p in
      let ty = P.type_info p in
      let pp = 
        List.fold_left
	  (fun p (coef, x) ->
            match X.ac_extract x with
              | Some ac when is_mult ac.h -> 
		P.add p (P.mult_const coef (unsafe_ac_to_arith ac))
              | _ -> 
		P.add p (P.create [coef,x] Q.zero ty)
	  ) (P.create [] c ty) l
      in
      if List.exists 
        (fun (_, x)-> 
	  match X.ac_extract x with | Some ac -> is_mult ac.h | _ -> false)
        (fst (P.to_list pp)) then p else pp

    let solve_aux r1 r2 =
      Debug.solve_aux r1 r2;
      let p1 = embed r1 in
      let p2 = embed r2 in
      let ty = P.type_info p2 in
      let p = P.add p1 (P.mult (P.create [] Q.m_one ty) p2) in
      let pp = safe_distribution p in
      if ty = Ty.Treal then solve_real pp else solve_int pp

    let solve_one r1 r2 =
      Options.tool_req 4 "TR-Arith-Solve";
      let sbs = solve_aux r1 r2 in
      let sbs = List.fast_sort (fun (a,_) (x,y) -> X.compare x a)sbs in
      Debug.solve_one r1 r2 sbs;
      sbs

    let apply_subst r l =
      List.fold_left (fun r (p,v) -> X.subst p v r) r l
        
    let triangular_down sbs = 
      List.fold_right
        (fun (p,v) nsbs -> (p, apply_subst v nsbs) :: nsbs) sbs []

    let make_idemp a b sbs = 
      let sbs = triangular_down sbs in
      let sbs = triangular_down (List.rev sbs) in (* triangular up *)
      let original = List.fold_right SX.add (X.leaves a) SX.empty in
      let original = List.fold_right SX.add (X.leaves b) original in
      let sbs = List.filter (fun (p,v) -> SX.mem p original) sbs in
      assert (not (Options.enable_assertions ()) ||
                X.equal (apply_subst a sbs) (apply_subst b sbs));
      sbs

    let solve r1 r2 pb = 
      let sbt = solve_one r1 r2 in
      {pb with sbt = List.rev_append (make_idemp r1 r2 sbt) pb.sbt}

    (*XXX*)

    let make t =
      if profiling() then
        try 
	  Options.exec_timer_start Timers.TArith;
	  let res = make t in
	  Options.exec_timer_pause Timers.TArith;
	  res
        with e -> 
	  Options.exec_timer_pause Timers.TArith;
	  raise e
      else make t

    let leaves p =
      if profiling() then
        try 
	  Options.exec_timer_start Timers.TArith;
	  let res = leaves p in
	  Options.exec_timer_pause Timers.TArith;
	  res
        with e -> 
	  Options.exec_timer_pause Timers.TArith;
	  raise e
      else leaves p

    let solve r1 r2 pb = 
      if profiling() then
        try 
	  Options.exec_timer_start Timers.TArith;
	  let res = solve r1 r2 pb in
	  Options.exec_timer_pause Timers.TArith;
	  res
        with e -> 
	  Options.exec_timer_pause Timers.TArith;
	  raise e
      else solve r1 r2 pb

    let print = P.print

    let fully_interpreted sb = 
      match sb with
        | Sy.Op (Sy.Plus | Sy.Minus) -> true
        | _ -> false

    let term_extract _ = None, false

    let abstract_selectors p acc =
      let p, acc = P.abstract_selectors p acc in
      is_mine p, acc

  end

module Relation = Fm.Relation
