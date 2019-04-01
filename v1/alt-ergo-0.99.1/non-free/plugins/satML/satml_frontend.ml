(******************************************************************************)
(*                               OCamlPro                                     *)
(*                                                                            *)
(* Copyright 2013-2014 OCamlPro                                               *)
(* All rights reserved. See accompanying files for the terms under            *)
(* which this file is distributed. In doubt, contact us at                    *)
(* contact@ocamlpro.com (http://www.ocamlpro.com/)                            *)
(*                                                                            *)
(******************************************************************************)

module Main = struct

  open Options
  open Format
  module Th = Theory.Main

  module Dfs_sat = Sat_solvers.Dfs_sat
  module SAT = Satml.Make(Th)
  module Inst = Matching.Make(Th)
  module Ex = Explanation
  module F = Formula
  module MF = F.Map
  module SF = F.Set
  module A = Literal.LT
  module T = Term
  module Types = Satml.Types
  module Hs = Hstring


  module FF = Satml.Flat_Formula
  module MFF = FF.Map
  module SFF = FF.Set

  let start () = SAT.start ()
  let stop () = SAT.stop ()

  type t = {
    nb_mrounds : int;
    gamma : int MF.t;
    conj : int MFF.t;
    abstr1 : (FF.t * Types.atom) MF.t;
    abstr2 : F.t MFF.t;
    inst : Inst.t;
    add_inst: Formula.t -> bool;
  }

  let empty () =
    SAT.empty (); (*(* Soundness issue due to bad hash-consing *) *)
    { gamma = MF.empty;
      nb_mrounds = 0;
      conj = MFF.empty;
      abstr1 = MF.empty;
      abstr2 = MFF.empty;
      inst = Inst.empty;
      add_inst = fun _ -> true;
    }
      
  let empty_with_inst add_inst =
    { (empty ()) with add_inst = add_inst }
      
  exception Sat of t
  exception Unsat of Explanation.t
  exception I_dont_know of t

  exception IUnsat of t * Explanation.t

  let mk_gf f = 
    { F.f = f;
      age = 0;
      lem = None;
      mf = false;
      gf = false;
      from_terms = []
    } 

  module Replay = struct

    let print_gamma env = 
      fprintf fmt "(* ground problem *)@.";
      MF.iter (fun f _ -> fprintf fmt "%a -> @." F.print f) env.gamma;
      fprintf fmt "false@."
        
    let replay_with_dfs env = 
      try
        let env_dfs = 
          try
            let env_dfs = 
              MF.fold
                (fun f _ env_dfs -> Dfs_sat.assume env_dfs (mk_gf f))
                env.gamma (Dfs_sat.empty ())
            in
            MF.fold
              (fun f (_,at) env_dfs ->
                let f = F.mk_iff f (F.mk_lit (Types.literal at) 0) 0 in
                Dfs_sat.assume env_dfs (mk_gf f)
              ) env.abstr1 env_dfs
          with Dfs_sat.Unsat dep -> raise (Unsat dep)
        in
        ignore (Dfs_sat.unsat env_dfs (mk_gf F.vrai));
        fprintf fmt "replay (by Dfs_sat.unsat)@."
          
      with
        | Unsat _ -> 
          fprintf fmt "replay (by Dfs_sat.assume)@.";
        | Dfs_sat.Unsat _ -> assert false
        | Dfs_sat.Sat _ ->
          fprintf fmt "satML said UNSAT but Dfs_sat said SAT@.";
          print_gamma env;
          exit 12
        | e ->
          fprintf fmt "satML said UNSAT but Dfs_sat said:@.";
          (*fprintf fmt "%s@." (Printexc.to_string e);*)
          exit 13
  end

  (*BISECT-IGNORE-BEGIN*)
  module Debug = struct
      
    let pred_def f = 
      if debug_sat () then
        eprintf "[sat] I assume a predicate: %a@.@." F.print f

    let unsat gf = 
      if debug_sat () then 
        printf "[sat] unsat of %a ?@." F.print gf.F.f

    let assume gf = 
      let {F.f=f;age=age;lem=lem;mf=mf;from_terms=terms} = gf in
      if debug_sat () then begin
        match F.view f with
	  | F.Unit _ -> ()
	    
	  | F.Clause _ -> 
	    fprintf fmt "[sat] I assume a clause %a@." F.print f
	      
	  | F.Lemma _ ->
	    fprintf fmt "[sat] I assume a [%d-atom] lemma: %a@."
              (F.size f) F.print f
	      
	  | F.Literal a -> 
	    Term.print_list str_formatter terms;
	    let s = flush_str_formatter () in
	    let n = match lem with 
	      | None -> ""  
	      | Some ff -> 
	        (match F.view ff with F.Lemma xx -> xx.F.name | _ -> "")
	    in
	    fprintf fmt "\n[sat]I assume a literal (%s : %s) %a@]@."
              n s Literal.LT.print a;
	    fprintf fmt "================================================@.@."
	      
	  | F.Skolem{F.sko_subst=(s,s_ty); sko_f=f} ->
	    fprintf fmt "[sat] I assume a skolem %a@." F.print f 
	      
	  | F.Let {F.let_var=lvar; let_term=lterm; let_f=lf} ->
	    fprintf fmt "[sat] I assume a let %a = %a in %a@." 
	      Symbols.print lvar Term.print lterm F.print lf;
      end

    let simplified_form f f' = 
      if debug_sat () && verbose () then begin
        fprintf fmt "[sat] Simplified form of: %a@." F.print f;
        fprintf fmt "  is: %a@." FF.print f';
      end

    let cnf_form f unit non_unit =
      if debug_sat () && verbose () then begin
        fprintf fmt "[sat] CFF form of: %a@." FF.print f;
        fprintf fmt "  is:@.";
        List.iter 
          (List.iter (fun a -> fprintf fmt "UNIT: %a@." Types.pr_atom a))
          unit;
        List.iter
          (fun c ->
            fprintf fmt "CLAUSE: ";
            List.iter (fun a -> fprintf fmt "%a or " Types.pr_atom a) c;
            fprintf fmt "@."
          )non_unit
      end

    let model ()=
      if debug_sat () then 
        let model = SAT.boolean_model () in
        eprintf "@.(2) satML's model:@.";
        List.iter
          (fun a ->
            eprintf " %f | %a @." 
              (Types.weight a)
              Types.pr_atom a;
          ) (List.rev model);
        eprintf "  --------------@."

    let mround () = 
      if debug_sat () then fprintf fmt "matching round@."

    let new_instances env = 
      if debug_sat () then begin
        eprintf "@.# [sat] I GENERATE NEW INSTANCES ########################@.@.";
        eprintf "(1) ground problem: @.";
        MFF.iter (fun f md -> eprintf "-> %d : %a@." md FF.print f) env.conj;
        fprintf fmt "@.Gamma:@.";
        model ();
      end

    let generated_instances l = 
      if verbose () && debug_sat () then begin
        eprintf "[new_instances] %d generated@." (List.length l);
        List.iter (fun ({F.f=f}, _) -> eprintf " instance: %a@." F.print f) l
      end

    let trivial_fact p inst = 
      if verbose () && debug_sat () then begin
        if inst then eprintf "already known instance: %a@." F.print p
        else eprintf "already known skolem: %a@." F.print p
      end

    let generated_skolems l = 
      if verbose () && debug_sat () then begin
        eprintf "[new_skolems] %d generated@." (List.length l);
        List.iter (fun {F.f=f} -> eprintf " skolem: %a@." F.print f) l
      end

    let terms_from_sat_branch f = 
      if verbose () && debug_sat () then begin
        fprintf fmt "[extract_and_add_terms from] %a@." FF.print f;
      end

    let add_terms_of src terms = 
      if verbose () && debug_sat () then begin
        fprintf fmt "[%s] add_terms_of:@." src;
        Term.Set.iter (fprintf fmt ">> %a@." Term.print) terms;
        fprintf fmt "@.";
      end

    let axiom_def f = 
      if debug_sat () then
        eprintf "[sat] I assume an axiom: %a@.@." F.print f
          
    let internal_axiom_def f fa =
      if debug_sat () then
        eprintf "[sat] I assume an internal axiom: %a <-> %a@.@."
          FF.print fa F.print f
          
  end
  (*BISECT-IGNORE-END*)

  let selector env f orig =
    not (MF.mem f env.gamma)
    && begin match F.view orig with
      | F.Lemma _ -> env.add_inst orig
      | _ -> true
    end

  let mround env = 
    let tbox = SAT.current_tbox () in
    let gd2, ngd2 = Inst.m_predicates env.inst tbox (selector env) in 
    let l2 = List.rev_append (List.rev gd2) ngd2 in
    (*let env = assume env l2 in*)
    let gd1, ngd1 = Inst.m_lemmas env.inst tbox (selector env) in 
    let l1 = List.rev_append (List.rev gd1) ngd1 in
    let l = ((List.rev_append l2 l1) : (F.gformula * Explanation.t) list) in
    List.rev_map
      (fun (gf,dep) ->
        let orig = match gf.F.lem with None -> assert false | Some lem -> lem in
        try
          let _, at = MF.find orig env.abstr1 in
          assert (Types.is_true at && Types.level at >= 0);
          (*if at.ST.var.ST.level = 0 then 
            (p, dep, w) :: acc 
            else*)
          let fat = F.mk_lit (Types.literal at) 0 in
          let f' = F.mk_or (F.mk_not fat) gf.F.f 0 in
          ({gf with F.f = f' }, dep)
        with
          | Not_found -> (gf, dep)
      )l

  let print_propositional_model () =
    let model = SAT.boolean_model () in
    fprintf fmt "Propositional:";
    List.iter
      (fun at ->
        (fprintf fmt "\n %a" Literal.LT.print) (Types.literal at)
      ) model;
    fprintf fmt "\n@."

  let print_model ~header fmt env =
    Format.print_flush ();
    if header then fprintf fmt "\nModel\n@.";
    print_propositional_model ();
    Th.print_model fmt (SAT.current_tbox ())

  let make_explanation lc = Ex.empty (*
                                       if debug_sat () then 
                                       fprintf fmt "make_explanation of %d clauses@." (List.length lc);
                                       List.fold_left
                                       (fun ex ({ST.form = f} as c) ->
                                       if debug_sat () then
                                       fprintf fmt "unsat_core: %a@." Types.pr_clause c;
                                       Ex.union (Ex.singleton (Ex.Dep f)) ex
                                       )Ex.empty lc*)

  let pred_def env f = 
    let gf = mk_gf f in
    Debug.pred_def f;
    {env with inst = Inst.add_predicate env.inst gf}

  let axiom_def env gf ex = 
    {env with inst = Inst.add_lemma env.inst gf ex}

  let register_abstraction env (f, (af, at)) = 
    if debug_sat () && verbose () then
      fprintf fmt "abstraction: %a  --> %a@." F.print f FF.print af;
    if MF.mem f env.abstr1 then begin
      let _, bt = MF.find f env.abstr1 in
      if Types.cmp_atom at bt <> 0 then begin
        fprintf fmt "%a -----> %a@.ET@.%a -----> %a@."
          F.print f Types.pr_atom at 
          F.print f Types.pr_atom bt;
        assert false
      end;
    end;
    let gf = mk_gf f in
    { env with
      inst = 
        if not (Types.is_true at) then env.inst
        else Inst.add_lemma env.inst gf Ex.empty;
      abstr1 = MF.add f (af, at) env.abstr1;
      abstr2 = MFF.add af f env.abstr2
    }

  let internal_axiom_def ax fa inst = 
    Debug.internal_axiom_def ax fa;
    let gax = mk_gf ax in
    Inst.add_lemma inst gax Ex.empty

  let terms_from_atom f env (inst, acc, sa) a = 
    let gf = mk_gf F.vrai in
    if A.Set.mem a sa then inst, acc, sa
    else
      let sa = A.Set.add a sa in
      if verbose () then
        fprintf fmt "terms_of_atom %a @.@." Literal.LT.print a;
      let inst = Inst.add_terms inst (A.terms_of a) gf in
      let fa = FF.mk_lit a in
      (* ax <-> fa, if ax exists in abstr2 *)
      try
        let ax = MFF.find fa env.abstr2 in
        internal_axiom_def ax fa inst, acc, sa
      with Not_found -> 
        try
          (* ax <-> fa  donc ax -> fa i.e (not ax) or fa *)
          let ax = MFF.find (FF.mk_not fa) env.abstr2 in
          match F.view (F.mk_not ax) with
            | F.Skolem {F.sko_subst=s; sko_f=fs} -> 
              let neg_ax = F.apply_subst s fs in
              let f = F.mk_or (F.mk_not (F.mk_lit a 0)) neg_ax 0 in
              if MF.mem f env.gamma (*|| is_satisfied env p*) then begin
                Debug.trivial_fact f false;
                inst, acc, sa
              end
              else inst, f :: acc, sa
            | _ -> assert false
        with Not_found -> inst, acc, sa

  let measure at = 
    Types.level  at,
    Types.weight at,
    Types.index  at

  (* smaller is more important *)
  let cmp_tuples (l1, w1, i1) (l2,w2, i2) =
    (* lower decision level is better *)
    let res = compare l1 l2 in
    if res <> 0 then res 
    else
      (* higher weight is better hence compare w2 w1 *)
      let res = compare w2 w1 in
      if res <> 0 then res
      else
        (* lower index is better *)
        compare i1 i2

  let max a b = if cmp_tuples a b > 0 then a else b
      
  let take_max aux l =
    let ((lvl, _, ind) ,_) as acc = 
      List.fold_left (fun ((mz,lz) as acc) f ->
        match aux f with
          | None -> acc
          | Some (m, l) -> 
            if cmp_tuples m mz > 0 then (m, l) else acc
      )((-1, -.1., -1), []) l
    in
    if lvl = -1 && ind = -1 then None
    else Some acc

  let take_min aux l = 
    let ((lvl, _, ind) ,_) as acc = 
      List.fold_left (fun ((mz,lz) as acc) f ->
        match aux f with
          | None -> acc
          | Some (m, l) -> 
            if cmp_tuples m mz < 0 then (m, l) else acc
      )((max_int, -.1., max_int), []) l
    in
    if lvl = max_int && ind = max_int then None
    else Some acc

  let rec take_normal aux l =
    match l with
        [] -> None
      | a::l ->
        match aux a with
          | None -> take_normal aux l
          | (Some _) as v -> v

  let terms_from_sat_branches =
    let rec terms_from_sat_branch f = 
      match FF.view f with
        | FF.UNIT at -> 
          if not (Types.is_true at) then None
          else Some (measure at, [Types.literal at])

        | FF.AND l -> 
          begin 
            try
              let acc = 
                List.fold_left (fun (mz,lz) f ->
                  match terms_from_sat_branch f with
                    | None -> raise Exit
                    | Some (m, l) -> max m mz, List.rev_append l lz
                )((-1, -.1., -1), []) l
              in
              Some acc
            with Exit -> None
          end

        | FF.OR l ->
          take_normal terms_from_sat_branch l
    in
    fun env ->
      let inst, acc, sa = 
        MFF.fold
          (fun f _ (inst, acc, sa) ->
            Debug.terms_from_sat_branch f;
            match terms_from_sat_branch f with
              | None   -> assert false
              | Some (_,l) -> 
                List.fold_left
                  (fun (inst, acc, sa) a -> 
                    terms_from_atom f env (inst, acc, sa) a)
                  (inst, acc, sa) l
          ) env.conj (env.inst, [], A.Set.empty)
      in
      inst, acc
        
  (* XXX not used yet *)
  let terms_from_bmodel2 env = 
    MF.fold
      (fun f _ inst -> Inst.add_terms inst (F.terms f) (mk_gf f)) 
      env.gamma env.inst

  (* XXX greedy: does not add ground terms of quantified formulas*)
  let terms_from_bmodel env = 
    let bmodel = SAT.boolean_model () in
    List.fold_left
      (fun (inst, acc) at ->
        let a = Types.literal at in
        let inst,acc,_ = 
          terms_from_atom F.vrai env (inst, acc, A.Set.empty) a in
        inst, acc
      ) (env.inst, []) bmodel

  let terms_from_sat_branches env = 
    if greedy () then terms_from_bmodel env
    else terms_from_sat_branches env

  let terms_from_dec_proc env = 
    let terms = Th.extract_ground_terms (SAT.current_tbox ()) in
    Debug.add_terms_of "terms_from_dec_proc" terms;
    let gf = mk_gf F.vrai in
    Inst.add_terms env.inst terms gf

  let new_instances env = 
    Debug.new_instances env;
    let inst, acc = terms_from_sat_branches env in 
    let acc = List.map mk_gf acc in
    let inst = terms_from_dec_proc {env with inst=inst} in
    let l = mround {env with inst = inst} in
    Debug.generated_instances l; 
    Debug.generated_skolems acc; 
    let l = List.map (fun (gf, dep) -> gf) l in
    List.rev_append acc l

  let assume_aux (env, updated) gf =
    let {F.f=f} = gf in
    if MF.mem f env.gamma then env, updated
    else
      let env = {env with gamma = MF.add f env.nb_mrounds env.gamma} in
      Debug.assume gf;
      let env = match F.view f with
        | F.Lemma _ -> axiom_def env gf Ex.empty
        | _ ->
          let f', axs = FF.simplify f (fun f -> MF.find f env.abstr1) in
          Debug.simplified_form f f';
          let env = { env with conj = MFF.add f' env.nb_mrounds env.conj } in
          let env = List.fold_left register_abstraction env axs in
          let unit, nunit, proxies = FF.cnf f' in
          Debug.cnf_form f' unit nunit;
          try 
            SAT.assume unit f ~cnumber:0;
            SAT.assume nunit f ~cnumber:0;
            env
          with 
            | Satml.Unsat (lc)  -> raise (IUnsat (env, make_explanation lc))
            | Satml.Sat -> assert false
      in
      env, true

  let rec unsat_rec env : unit =
    try SAT.solve (); assert false
    with 
      | Satml.Unsat lc -> raise (IUnsat (env, make_explanation lc))
      | Satml.Sat ->
        let l = new_instances env in 
        let env = {env with nb_mrounds = env.nb_mrounds + 1} in
        let env, updated = List.fold_left assume_aux (env, false) l in
        if not updated then raise (I_dont_know env);
        unsat_rec env

  let unsat env gf = 
    Debug.unsat gf;
    (* In dfs_sat goals' terms are added to env.inst *)
    let env = {env with inst = Inst.add_terms env.inst (F.terms gf.F.f) gf} in
    try 
      let env, updated = List.fold_left assume_aux (env, false) [gf] in
      unsat_rec env;
      assert false
    with IUnsat (env, dep) -> 
      if replay_satml_dfs () then Replay.replay_with_dfs env;
      dep

  let assume env gf =
    try fst (assume_aux (env, false) gf)
    with IUnsat (env, dep) -> raise (Unsat dep)
      
  let retrieve_used_context {inst=inst} = Inst.retrieve_used_context inst
end

let _ = Sat_solvers.set_current (module Main : Sat_solvers.S)
