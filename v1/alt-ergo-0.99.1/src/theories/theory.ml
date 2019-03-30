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
open Exception

module X = Combine.Shostak
module Ex = Explanation
module SetF = Formula.Set
module T = Term
module A = Literal
module LR = A.Make(struct type t = X.r include X end)
module SetT = Term.Set
module Sy = Symbols
module SetX = Set.Make(struct type t = X.r let compare = X.compare end)

module CcX : sig

  type t
  type r = Combine.Shostak.r
  val empty : unit -> t
  val add_term : t ->
    (r Sig.literal * Explanation.t) list ->
    Term.t -> Explanation.t -> t * (r Sig.literal * Explanation.t) list
  val add :
    t ->
    (r Sig.literal * Explanation.t) list ->
    Literal.LT.t ->
    Explanation.t -> t * (r Sig.literal * Explanation.t) list
  val assume_literal :
    t ->
    (r Sig.literal * Explanation.t) list ->
    (r Sig.literal * Explanation.t) list ->
    t * (r Sig.literal * Explanation.t) list
      
  val case_split : t -> (r Literal.view * Explanation.t * Numbers.Q.t) list
  val query :  t -> Literal.LT.t -> Sig.answer
  val new_terms : t -> Term.Set.t
  val class_of : t -> Term.t -> Term.t list
  val are_equal : t -> Term.t -> Term.t -> Sig.answer
  val are_distinct : t -> Term.t -> Term.t -> Sig.answer
  val cl_extract : t -> Term.Set.t list
  val term_repr : t -> Term.t -> Term.t
  val print_model : Format.formatter -> t -> unit

end = struct

  module SetA = Use.SA
  module Use = Combine.Use
  module Uf = Combine.Uf
  module Rel = Combine.Relation

  type t = { 
    use : Use.t;  
    uf : Uf.t ;
    relation : Rel.t
  }

  type r = Combine.Shostak.r

  let empty () = {  
    use = Use.empty ; 
    uf = Uf.empty () ; 
    relation = Rel.empty [];
  }
    
  (*BISECT-IGNORE-BEGIN*)
  module Debug = struct

    let cc r1 r2 =
      if debug_cc () then 
	fprintf fmt "[cc] congruence closure : %a = %a@." 
	  X.print r1 X.print r2

    let make_cst t ctx =
      if debug_cc () then 
	if ctx = [] then ()
	else begin
          fprintf fmt "[cc] constraints of make(%a)@." Term.print t;
          let c = ref 0 in
          List.iter 
            (fun a ->
              incr c;
              fprintf fmt " %d) %a@." !c A.LT.print a) ctx
	end

    let add_to_use t = 
      if debug_cc () then 
	fprintf fmt "[cc] add_to_use: %a@." T.print t
	  
    let lrepr fmt = List.iter (fprintf fmt "%a " X.print)

    let leaves t lvs = 
      fprintf fmt "[cc] leaves of %a@.@." 
	T.print t; lrepr fmt lvs
	  
    let contra_congruence a ex = 
      if debug_cc () then 
	fprintf fmt "[cc] find that %a %a by contra-congruence@." 
	  A.LT.print a Ex.print ex

    let assume_literal sa =
      if debug_cc () then
	fprintf fmt "[cc] assume literal : %a@." LR.print (LR.make sa)

    let congruent a ex =
      if debug_cc () then
	fprintf fmt "[cc] new fact by conrgruence : %a ex[%a]@." 
          A.LT.print a Ex.print ex
  end
  (*BISECT-IGNORE-END*)

  let one, _ = X.make (Term.make (Sy.name "@bottom") [] Ty.Tint)

  let concat_leaves uf l = 
    let rec concat_rec acc t = 
      match  X.leaves (fst (Uf.find uf t)) , acc with
	  [] , _ -> one::acc
	| res, [] -> res
	| res , _ -> List.rev_append res acc
    in
    match List.fold_left concat_rec [] l with
	[] -> [one]
      | res -> res

  let are_equal env ex t1 t2 = 
    if T.equal t1 t2 then ex
    else match Uf.are_equal env.uf t1 t2 with
      | Yes (dep, _) -> Ex.union ex dep
      | No -> raise Exit

  let equal_only_by_congruence env ex t1 t2 acc = 
    if T.equal t1 t2 then acc
    else
      let {T.f=f1; xs=xs1; ty=ty1} = T.view t1 in
      if X.fully_interpreted f1 then acc
      else 
	let {T.f=f2; xs=xs2; ty=ty2} = T.view t2 in
        if Symbols.equal f1 f2 && Ty.equal ty1 ty2 then
	  try
            let ex = List.fold_left2 (are_equal env) ex xs1 xs2 in
            let a = A.LT.mk_eq t1 t2 in
            Debug.congruent a ex;
            (LTerm a, ex) :: acc
          with Exit -> acc
        else acc

  let congruents env t1 s acc ex = 
    SetT.fold (equal_only_by_congruence env ex t1) s acc

  let fold_find_with_explanation find ex l = 
    List.fold_left 
      (fun (lr, ex) t -> 
	let r, ex_r = find t in r::lr, Ex.union ex_r ex)
      ([], ex) l

  let view find va ex_a = 
    match va with
      | A.Pred (t1, b) ->
        let r1, ex1 = find t1 in
	let ex = Ex.union ex1 ex_a in
	LR.mkv_pred r1 b, ex
      | A.Eq (t1, t2) ->
        let r1, ex1 = find t1 in
	let r2, ex2 = find t2 in
	let ex = Ex.union (Ex.union ex1 ex2) ex_a in
	LR.mkv_eq r1 r2, ex
      | A.Distinct (b, lt) -> 
	let lr, ex = fold_find_with_explanation find ex_a lt in 
	LR.mkv_distinct b (List.rev lr), ex
      | A.Builtin(b, s, l) -> 
	let lr, ex  = fold_find_with_explanation find ex_a l in
	LR.mkv_builtin b s (List.rev lr), ex
          
  let term_canonical_view env a ex_a =  
    view (Uf.find env.uf) (A.LT.view a) ex_a

  let canonical_view env a ex_a = view (Uf.find_r env.uf) a ex_a

  let new_facts_by_contra_congruence env r bol ex = 
    match X.term_extract r with
      | None, _ | Some _, false -> []
      | Some t1, true ->  (* original term *)
	match T.view t1 with
	  | {T.f=f1 ; xs=[x]} -> 
            let ty_x = (Term.view x).Term.ty in
	    List.fold_left 
	      (fun acc t2 ->
		match T.view t2 with
		  | {T.f=f2 ; xs=[y]} when Sy.equal f1 f2 ->
                    let ty_y = (Term.view y).Term.ty in
                    if Ty.equal ty_x ty_y then
		      let a = 
                        A.LT.mk_distinct false [x; y] in
		      let dist = LTerm a in
		      begin match Uf.are_distinct env.uf t1 t2 with
		        | Yes (ex', _) -> 
			  let ex_r = Ex.union ex ex' in
			  Debug.contra_congruence a ex_r;
			  (dist, ex_r) :: acc
		        | No -> assert false
		      end
                    else acc
		  | _ -> acc
	      ) [] (Uf.class_of env.uf bol)
	  | _ -> []

  let contra_congruence  = 
    Options.exec_thread_yield ();
    let vrai,_ = X.make T.vrai in
    let faux, _ = X.make T.faux in
    fun env r ex -> 
      if X.equal (fst (Uf.find_r env.uf r)) vrai then
	new_facts_by_contra_congruence env r T.faux ex
      else if X.equal (fst (Uf.find_r env.uf r)) faux then
	new_facts_by_contra_congruence env r T.vrai ex
      else []

  let clean_use = 
    List.fold_left 
      (fun env (a, ex) -> 
	match a with 
	  | LSem _ -> assert false
	  | LTerm t -> 
	    begin
	      match A.LT.view t with
		| A.Distinct (_, lt) 
		| A.Builtin (_, _, lt) ->
		  let lvs = concat_leaves env.uf lt in
		  List.fold_left
		    (fun env rx ->
		      let st, sa = Use.find rx env.use in
		      let sa = SetA.remove (t, ex) sa in
		      { env with use = Use.add rx (st,sa) env.use }
		    ) env lvs
		| _ -> assert false
	    end) 

  let rec congruence_closure env r1 r2 ex = 
    Options.exec_thread_yield ();
    Debug.cc r1 r2;
    let uf, res = Uf.union env.uf r1 r2 ex in
    List.fold_left 
      (fun (env, l) (p, touched, v) ->
	Options.exec_thread_yield ();
	(* we look for use(p) *)
      	let p_t, p_a = Use.find p env.use in
	
	(* we compute terms and atoms to consider for congruence *)
	let repr_touched = List.map (fun (_,a,_) -> a) touched in
	let st_others, sa_others = 
          Use.congr_close_up env.use p repr_touched in
	
	(* we update use *)
	let nuse = Use.up_close_up env.use p v in
	Use.print nuse;
	
	(* we check the congruence of the terms. *)
	let env =  {env with use=nuse} in
	let new_eqs = 
	  SetT.fold 
            (fun t l -> congruents env t st_others l ex) p_t l in
       	let touched_atoms = 
	  List.map (fun (x,y,e)-> (LSem(LR.mkv_eq x y), e)) touched 
	in
	let touched_atoms = SetA.fold (fun (a, ex) acc ->
	  (LTerm a, ex)::acc) p_a touched_atoms in
	let touched_atoms = SetA.fold (fun (a, ex) acc ->
	  (LTerm a, ex)::acc) sa_others touched_atoms in
	env, (new_eqs @ touched_atoms)
	  
      ) ({env with uf=uf}, [])  res

  let replay_atom env sa = 
    Options.exec_thread_yield ();
    let relation, result = Rel.assume env.relation env.uf sa in
    let env = { env with relation = relation } in
    let env = clean_use env result.remove in
    env, result.assume

  let rec add_term env choices t ex =
    Options.exec_thread_yield ();
    (* nothing to do if the term already exists *)
    if Uf.mem env.uf t then env, choices
    else begin
      Options.tool_req 3 "TR-CCX-AddTerm";
      Debug.add_to_use t;

      (* we add t's arguments in env *)
      let {T.f = f; xs = xs} = T.view t in
      let env, choices = 
	List.fold_left (fun (env, ch) t -> add_term env ch t ex)
	  (env, choices) xs 
      in
      (* we update uf and use *)
      let nuf, ctx  = Uf.add env.uf t in 
      Debug.make_cst t ctx;
      let rt, _ = Uf.find nuf t in (* XXX : ctx only in terms *)
      let lvs = concat_leaves nuf xs in
      let nuse = Use.up_add env.use t rt lvs in
      
      (* If finitetest is used we add the term to the relation *)
      let rel = Rel.add env.relation rt in
      Use.print nuse;

      (* we compute terms to consider for congruence *)
      (* we do this only for non-atomic terms with uninterpreted 
         head-symbol *)
      let st_uset = Use.congr_add nuse lvs in
      
      (* we check the congruence of each term *)
      let env = {uf = nuf; use = nuse; relation = rel} in 
      let ct = congruents env t st_uset [] ex in
      let ct = (List.map (fun lt -> LTerm lt, ex) ctx) @ ct in
      assume_literal env choices ct
    end
      
  and add env choices a ex =
    match A.LT.view a with
      | A.Pred (t1, _) -> 
	add_term env choices t1 ex
      | A.Eq (t1, t2) -> 
	let env, choices = add_term env choices t1 ex in
	add_term env choices t2 ex
      | A.Distinct (_, lt) 
      | A.Builtin (_, _, lt) ->
	let env, choices =
          List.fold_left 
	    (fun (env, ch) t-> add_term env ch t ex)
            (env, choices) lt
        in
	let lvs = concat_leaves env.uf lt in (* A verifier *)
	let env = List.fold_left
	  (fun env rx ->
	    let st, sa = Use.find rx env.use in
	    { env with 
	      use = Use.add rx (st,SetA.add (a, ex) sa) env.use }
	  ) env lvs
	in
	env, choices

  and semantic_view env choices la = 
    Options.exec_thread_yield ();
    List.fold_left 
      (fun (env, choices, lsa) (a, ex) ->
	match a with
	  | LTerm a -> 
	    let env, choices = add env choices a ex in
	    let sa, ex = term_canonical_view env a ex in
	    env, choices, (sa, Some a, ex)::lsa

          (* XXX si on fait canonical_view pour
	     A.Distinct, la theorie des tableaux
             part dans les choux *)
	  | LSem (A.Builtin _  (*| A.Distinct _*) as sa) ->
	    let sa, ex = canonical_view env sa ex in
	    env, choices, (sa, None, ex)::lsa
	  | LSem sa ->
	    env, choices, (sa, None, ex)::lsa)
      (env, choices, []) la


  and assume_literal env choices la =
    Options.exec_thread_yield ();
    if la = [] then env, choices
    else
      let env, choices, lsa = semantic_view env choices la in
      let env, choices =
	List.fold_left
	  (fun acc (sa, _, ex) ->
	    Debug.assume_literal sa;
	    match sa with
              | A.Pred (r1,neg) ->
                let r2, r3 = 
                  if neg then X.bot(), X.top () else X.top (), X.bot()
                in
                let acc = assume_eq r1 r2 acc ex in
                assume_dist acc [r1;r3] ex
                  
	      | A.Eq(r1, r2) ->
                assume_eq r1 r2 acc ex
	          
              | A.Distinct (false, lr) ->
                assume_dist acc lr ex

	      | A.Distinct (true, _) -> assert false
	      | A.Builtin _ ->
		Options.tool_req 3 "TR-CCX-Builtin";
		acc
          ) (env, choices) lsa
      in
      let env, l = replay_atom env lsa in
      assume_literal env (choices@l) l

  and assume_eq r1 r2 (env, choices) ex =
    Options.tool_req 3 "TR-CCX-Congruence";
    let env, l = congruence_closure env r1 r2 ex in
    let env, choices = assume_literal env choices l in
    if Options.nocontracongru () then env, choices
    else 
      let env, choices = 
	assume_literal env choices (contra_congruence env r1 ex) 
      in
      assume_literal env choices (contra_congruence env r2 ex)

  and assume_dist (env, choices) lr ex =
    Options.tool_req 3 "TR-CCX-Distinct";
    if Uf.already_distinct env.uf lr then env, choices
    else 
      {env with uf = Uf.distinct env.uf lr ex}, choices



  (*************)

  let case_split env = Rel.case_split env.relation

  let query env a = 
    let ra, ex_ra = term_canonical_view env a Ex.empty in
    Rel.query env.relation env.uf (ra, Some a, ex_ra) 

  let new_terms env = Rel.new_terms env.relation 

  let class_of env t = Uf.class_of env.uf t

  let are_equal env t1 t2 = Uf.are_equal env.uf t1 t2

  let are_distinct env t1 t2 = Uf.are_distinct env.uf t1 t2

  let cl_extract env = Uf.cl_extract env.uf

  let term_repr env t = Uf.term_repr env.uf t

  let print_model fmt env =
    let zero = ref true in
    let eqs, neqs = Uf.model env.uf in
    let rs = 
      List.fold_left (fun acc (r, l, to_rel) ->
	if l <> [] then begin
	  if !zero then begin 
	    fprintf fmt "Theory:";
	    zero := false;
	  end;
	  fprintf fmt "\n %a = %a" (T.print_list_sep " = ") l X.print r;
	end;
	to_rel@acc
      ) [] eqs in
    List.iter (fun lt ->
      if !zero then begin 
	fprintf fmt "Theory:";
	zero := false;
      end;
      fprintf fmt "\n %a" (T.print_list_sep " <> ") lt;
    ) neqs;
    if not !zero then fprintf fmt "\n@.";
    Rel.print_model fmt env.relation rs

end



module type S = sig
  type t

  val empty : unit -> t
  val assume : Literal.LT.t -> Explanation.t -> t -> t * Term.Set.t * int
  val query : Literal.LT.t -> t -> answer
  val class_of : t -> Term.t -> Term.t list
  val are_equal : t -> Term.t -> Term.t -> answer
  val print_model : Format.formatter -> t -> unit
  val cl_extract : t -> Term.Set.t list
  val term_repr : t -> Term.t -> Term.t
  val extract_ground_terms : t -> Term.Set.t
end

module Main = struct    

  (*BISECT-IGNORE-BEGIN*)
  module Debug = struct
      
    let begin_case_split () = 
      if debug_split () then
	fprintf fmt "============= Begin CASE-SPLIT ===============@."

    let end_case_split () = 
      if debug_split () then
	fprintf fmt "============= End CASE-SPLIT ===============@."

    let split_size sz = 
      if debug_split () then
	fprintf fmt ">size case-split: %s@." (Numbers.Q.string_of sz)

    let print_lr_view fmt ch = LR.print fmt (LR.make ch)

    let split_backtrack neg_c ex_c = 
      if debug_split () then
        fprintf fmt "[case-split] I backtrack on %a : %a@."
          print_lr_view neg_c Ex.print ex_c

    let split_assume c ex_c =
      if debug_split () then
        fprintf fmt "[case-split] I assume %a : %a@."
          print_lr_view c Ex.print ex_c

    let split_backjump c dep =
      if debug_split () then
        fprintf fmt "[case-split] I backjump on %a : %a@."
          print_lr_view c Ex.print dep

    let query a =
      if debug_cc () then fprintf fmt "[cc] query : %a@." A.LT.print a

    let split () = 
      if debug_split () then
        fprintf fmt "[case-split] I replay choices@."
  end
  (*BISECT-IGNORE-END*)

  type choice_sign =
    | CPos of Ex.exp (* The explication of this choice *)
    | CNeg (* The choice has been already negated *)


  type t = { 
    terms : Term.Set.t;
    gamma : CcX.t;
    gamma_finite : CcX.t;
    choices : (X.r Literal.view * Numbers.Q.t * choice_sign * Ex.t) list; 
  (** the choice, the size, choice_sign,  the explication set,
      the explication for this choice. *)
  }

  let look_for_sat ?(bad_last=No) ch t base_env l =
    let rec aux ch bad_last dl base_env li = 
      Options.exec_thread_yield ();
      match li, bad_last with
        | [], _ -> 
	  begin
	    Options.tool_req 3 "TR-CCX-CS-Case-Split";
            match CcX.case_split base_env with
	      | [] -> 
		{ t with gamma_finite = base_env; choices = List.rev dl }, ch
	      | l ->
	        let l = 
		  List.map
		    (fun (c, ex_c, size) ->
                      let exp = Ex.fresh_exp () in
                      let ex_c_exp = Ex.add_fresh exp ex_c in
                      (* A new explanation in order to track the choice *)
                      (c, size, CPos exp, ex_c_exp)) l in
	        let sz =
		  List.fold_left
		    (fun acc (a,s,_,_) ->
		      Numbers.Q.mult acc s) (Numbers.Q.one) (l@dl) in
                Debug.split_size sz;
	        if Numbers.Q.compare sz (max_split ()) <= 0  || 
		  Numbers.Q.sign  (max_split ()) < 0 then
		  aux ch No dl base_env l
	        else
		  { t with gamma_finite = base_env; choices = List.rev dl }, ch
	  end
        | ((c, size, CNeg, ex_c) as a)::l, _ ->
	  let base_env, ch = CcX.assume_literal base_env ch [LSem c, ex_c] in
	  aux ch bad_last (a::dl) base_env l

        (** This optimisation is not correct with the current explanation *)
        (* | [(c, size, CPos exp, ex_c)], Yes (dep,_) -> *)
        (*     let neg_c = CcX.Rel.choice_mk_not c in *)
        (*     let ex_c = Ex.union ex_c dep in *)
        (*     Debug.split_backtrack neg_c ex_c; *)
        (*     aux ch No dl base_env [neg_c, Numbers.Q.Int 1, CNeg, ex_c] *)

        | ((c, size, CPos exp, ex_c_exp) as a)::l, _ ->
	  try
            Debug.split_assume c ex_c_exp;
	    let base_env, ch = 
              CcX.assume_literal base_env ch [LSem c, ex_c_exp] in
	    Options.tool_req 3 "TR-CCX-CS-Normal-Run";
	    aux ch bad_last (a::dl) base_env l
	  with Exception.Inconsistent (dep, classes) ->
            match Ex.remove_fresh exp dep with
              | None ->
                (* The choice doesn't participate to the inconsistency *)
                Debug.split_backjump c dep;
		Options.tool_req 3 "TR-CCX-CS-Case-Split-Conflict";
                raise (Exception.Inconsistent (dep, classes))
              | Some dep ->
		Options.tool_req 3 "TR-CCX-CS-Case-Split-Progress";
                (* The choice participates to the inconsistency *)
                let neg_c = LR.view (LR.neg (LR.make c)) in
	        Debug.split_backtrack neg_c dep;
		if bottom_classes () then
		  printf "bottom (case-split):%a\n@." 
		    Term.print_tagged_classes classes;
		aux ch No dl base_env [neg_c, Numbers.Q.one, CNeg, dep]
    in
    aux ch bad_last (List.rev t.choices) base_env l

  let try_it f t =
    Options.exec_thread_yield ();
    Debug.begin_case_split ();
    let r =
      try 
	if t.choices = [] then look_for_sat [] t t.gamma []
	else
	  try
	    let env, ch = f t.gamma_finite in
	    look_for_sat ch t env []
	  with Exception.Inconsistent (dep, classes) -> 
            Debug.split ();
	    Options.tool_req 3 "TR-CCX-CS-Case-Split-Erase-Choices";
	    (* we replay the conflict in look_for_sat, so we can
	       safely ignore the explanation which is not useful *)
	    look_for_sat ~bad_last:(Yes (dep, classes))
	      [] { t with choices = []} t.gamma t.choices
      with Exception.Inconsistent (d, cl) ->
	Debug.end_case_split ();
	Options.tool_req 3 "TR-CCX-CS-Conflict";
	raise (Exception.Inconsistent (d, cl))
    in
    Debug.end_case_split (); r


  let extract_from_semvalues acc l =
    List.fold_left
      (fun acc r -> 
	match X.term_extract r with 
	  | Some t, _ -> SetT.add t acc | _ -> acc) acc l

  let extract_terms_from_choices =
    List.fold_left 
      (fun acc (a, _, _, _) -> 
        match a with
          | A.Eq(r1, r2) -> extract_from_semvalues acc [r1; r2]
          | A.Distinct (_, l) -> extract_from_semvalues acc l
          | A.Pred(p, _) -> extract_from_semvalues acc [p]
          | _ -> acc
      )

  let extract_terms_from_assumed = 
    List.fold_left 
      (fun acc (a, _) -> 
	match a with
	  | LTerm r -> begin
	    match Literal.LT.view r with 
	      | Literal.Eq (t1, t2) -> 
		SetT.add t1 (SetT.add t2 acc)
	      | Literal.Distinct (_, l) | Literal.Builtin (_, _, l) -> 
		List.fold_right SetT.add l acc
	      | Literal.Pred (t1, _) -> 
		SetT.add t1 acc

	  end
	  | _ -> acc)

  let assume a ex t = 
    let a = LTerm a in
    let gamma, ch = CcX.assume_literal t.gamma [] [a, ex] in
    let t = { t with gamma = gamma } in
    let t, ch = try_it (fun env -> CcX.assume_literal env ch [a, ex] ) t  in 
    let choices = extract_terms_from_choices SetT.empty t.choices in
    let choices_terms = extract_terms_from_assumed choices ch in
    let new_terms = CcX.new_terms t.gamma in
    t, T.Set.union choices_terms new_terms, 1

  let class_of t term = CcX.class_of t.gamma term

  let add_and_process a t =
    Options.exec_thread_yield ();
    let aux a ex env = 
      let gamma, l = CcX.add env [] a ex in CcX.assume_literal gamma [] l
    in
    let gamma, _ = aux a Ex.empty t.gamma in
    let t = { t with gamma = gamma } in
    let t, _ =  try_it (aux a Ex.empty) t in
    t

  let query a t =
    Options.exec_thread_yield ();
    Debug.query a;
    try
      match A.LT.view a with
	| A.Eq (t1, t2)  ->
	  let t = add_and_process a t in
          CcX.are_equal t.gamma t1 t2

	| A.Distinct (false, [t1; t2]) -> 
	  let na = A.LT.neg a in
	  let t = add_and_process na t in (* na ? *)
	  CcX.are_distinct t.gamma t1 t2
            
	| A.Distinct _ -> 
	  assert false (* devrait etre capture par une analyse statique *)

        | A.Pred (t1,b) ->
	  let t = add_and_process a t in
          if b 
          then CcX.are_distinct t.gamma t1 (Term.top())
          else CcX.are_equal t.gamma t1 (Term.top())

	| _ -> 
	  let na = A.LT.neg a in
	  let t = add_and_process na t in
          CcX.query t.gamma na
    with Exception.Inconsistent (d, classes) -> 
      Yes (d, classes)

  let are_equal t t1 t2 =  
    let gamma, _ = CcX.add_term t.gamma [] t1 Ex.empty in
    let gamma, _ = CcX.add_term gamma [] t2 Ex.empty in
    CcX.are_equal gamma t1 t2

  let empty () = 
    let env = CcX.empty () in
    let t = 
      { gamma = env;
        gamma_finite = env; 
        choices = []; 
        terms = Term.Set.empty }
    in
    let t, _, _ = 
      assume (A.LT.mk_distinct false [T.vrai; T.faux]) Ex.empty t
    in t

  let print_model fmt t = CcX.print_model fmt t.gamma_finite

  let cl_extract env = CcX.cl_extract env.gamma

  let term_repr env t = CcX.term_repr env.gamma t

  let assume a ex t = 
    let env, terms, cpt = 
      if profiling() then
        try 
	  Options.exec_timer_start Timers.TCC;
	  let res = assume a ex t in
	  Options.exec_timer_pause Timers.TCC;
	  res
        with e -> 
	  Options.exec_timer_pause Timers.TCC;
	  raise e
      else assume a ex t
    in
    {env with terms = Term.Set.union env.terms terms}, terms, cpt

  let query a t = 
    if profiling() then
      try 
	Options.exec_timer_start Timers.TCC;
	let res = query a t in
	Options.exec_timer_pause Timers.TCC;
	res
      with e -> 
	Options.exec_timer_pause Timers.TCC;
	raise e
    else query a t

  let class_of t term = 
    if profiling() then
      try 
	Options.exec_timer_start Timers.TCC;
	let res = class_of t term in
	Options.exec_timer_pause Timers.TCC;
	res
      with e -> 
	Options.exec_timer_pause Timers.TCC;
	raise e
    else class_of t term

  let extract_ground_terms env = env.terms

end
