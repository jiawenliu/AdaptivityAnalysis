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

module T = Term
module F = Formula
module MF = F.Map
module SF = F.Set
module Ex = Explanation
module MT = T.Map
module SubstT = Term.Subst

type gsubst = { 
  sbs : T.t SubstT.t;
  sty : Ty.subst;
  gen : int ;     (* l'age d'une substitution est l'age du plus vieux 
		     terme qu'elle contient *)
  goal : bool;    (* vrai si la substitution contient un terme ayant un lien 
		     avec le but de la PO *)
  s_term_orig : T.t list;
  s_lem_orig : F.t;
}

type trigger_info = {
  trigger_query : Literal.LT.t option ; 
  trigger_age : int ;  (* age d'un trigger *)
  trigger_orig : F.t ; (* lemme d'origine *)
  trigger_formula : F.t ; (* formule associee au trigger *)
  trigger_dep : Ex.t ;
}

type term_info = {
  term_age : int ;        (* age du terme *)
  term_from_goal : bool ;   (* vrai si le terme provient du but de la PO *)
  term_from_formula : F.t option; (* lemme d'origine du terme *)
  term_from_terms : T.t list;
}

module EMatch (X : Theory.S) = struct

  type info = {
    age : int ; (* age du terme *)
    lem_orig : F.t list ; (* lemme d'ou provient eventuellement le terme *)
    t_orig : T.t list; 
    but : bool  (* le terme a-t-il un lien avec le but final de la PO *)
  }

  type t = { 
    fils : T.t list MT.t SubstT.t ; 
    info : info MT.t ; 
    pats : (trigger_info * T.t list) list 
  }

  exception Echec
    
  let empty = { 
    fils = SubstT.empty ; 
    info = MT.empty ;
    pats = [ ];
  }

  let age_limite = Options.age_bound
  (* l'age limite des termes, au dela ils ne sont pas consideres par le
     matching *)

  (*BISECT-IGNORE-BEGIN*)
  module Debug = struct
      
    let add_term t = 
      if debug_matching() then
        fprintf fmt "[matching] add_term:  %a@." T.print t
          
    let matching pats = 
      if debug_matching() then begin
        fprintf fmt "@.[matching] (multi-)trigger: ";
        List.iter (fprintf fmt "%a , " T.print) pats;
        fprintf fmt "@.";
        fprintf fmt "========================================================@."
      end
        
    let match_pats_modulo pat lsubsts = 
      if debug_matching() then begin
        fprintf fmt "@.match_pat_modulo: %a  with accumulated substs@."
          T.print pat;
        List.iter (fun {sbs=sbs; sty=sty} ->
          fprintf fmt ">>> sbs= %a | sty= %a@." 
            (SubstT.print Term.print) sbs Ty.print_subst sty
        )lsubsts
      end
        
    let match_one_pat {sbs=sbs; sty=sty} pat0 = 
      if debug_matching() then
        fprintf fmt "@.match_pat: %a  with subst:  sbs= %a | sty= %a @."
          T.print pat0 (SubstT.print Term.print) sbs Ty.print_subst sty


    let match_one_pat_against {sbs=sbs; sty=sty} pat0 t = 
      if debug_matching() then
        fprintf fmt 
          "@.match_pat: %a  against term %a@.with subst:  sbs= %a | sty= %a @."
          T.print pat0 
          T.print t
          (SubstT.print Term.print) sbs
          Ty.print_subst sty

    let match_term {sbs=sbs; sty=sty} t pat =
      if debug_matching() then 
        fprintf fmt 
          "[match_term] I match %a against %a with subst: sbs=%a | sty= %a@."
          T.print pat T.print t (SubstT.print Term.print) sbs Ty.print_subst sty

    let match_list {sbs=sbs; sty=sty} pats xs =
      if debug_matching() then 
        fprintf fmt 
          "@.[match_list] I match %a against %a with subst: sbs=%a | sty= %a@."
          T.print_list pats 
          T.print_list xs 
          (SubstT.print Term.print) sbs
          Ty.print_subst sty

    let match_class_of t cl =
      if debug_matching() then 
        fprintf fmt "class_of (%a)  = { %a }@."
          T.print t
          (fun fmt -> List.iter (fprintf fmt "%a , " T.print)) cl
  end
  (*BISECT-IGNORE-END*)

  let infos op_gen op_but t g b env = 
    try 
      let i = MT.find t env.info in
      op_gen i.age g , op_but i.but b 
    with Not_found -> g , b

  let add_term info t env =
    Debug.add_term t;
    Options.exec_timer_start Timers.TMatch;
    let rec add_rec env t = 
      if MT.mem t env.info then env
      else
	let {T.f=f; xs=xs} = T.view t in
	let env = 
	  let map_f = try SubstT.find f env.fils with Not_found -> MT.empty in
	  
	  (* - l'age d'un terme est le min entre l'age passe en argument
	     et l'age dans la map 
	     - un terme est en lien avec le but de la PO seulement s'il
	     ne peut etre produit autrement (d'ou le &&)
	     - le lemme de provenance est le dernier lemme
	  *)
	  let g, b = infos min (&&) t info.term_age info.term_from_goal env in
	  let from_lems = 
	    List.fold_left 
	      (fun acc t ->
		try (MT.find t env.info).lem_orig @ acc
		with Not_found -> acc) 
	      (match info.term_from_formula with None -> [] | Some a -> [a]) 
	      info.term_from_terms
	  in
	  { env with
	    fils = SubstT.add f (MT.add t xs map_f) env.fils; 
	    info = 
	      MT.add t 
		{ age=g; lem_orig = from_lems; but=b; 
	          t_orig = info.term_from_terms } 
		env.info 
	  }
	in
	List.fold_left add_rec env xs
    in
    let env = if info.term_age > age_limite () then env else add_rec env t in
    Options.exec_timer_pause Timers.TMatch;
    env
      
  let add_trigger p trs env = { env with pats = (p, trs) ::env.pats }


  exception Deja_vu
  let deja_vu lem1 lems = 
    List.exists (fun lem2 -> F.compare lem1 lem2 = 0) lems

  let matching_loop_bound = 5

  module HF = Hashtbl.Make(F)

  let all_terms 
      f ty env pinfo 
      {sbs=s_t; sty=s_ty; gen=g; goal=b; 
       s_term_orig=s_torig; 
       s_lem_orig = s_lorig} lsbt_acc = 
    SubstT.fold 
      (fun k s l -> 
	MT.fold 
	  (fun t _ l -> 
	    try
	      let s_ty = Ty.matching s_ty ty (T.view t).T.ty in
	      let ng , but = 
		try 
		  let {age=ng;lem_orig=lem'; but=bt} = MT.find t env.info in
		  if deja_vu pinfo.trigger_orig lem' then raise Deja_vu;
		  max ng g , bt || b
		with Not_found -> g , b
	      in
	      { sbs = SubstT.add f t s_t;
		sty = s_ty;
		gen = ng; 
		goal = but;
		s_term_orig = t :: s_torig;
		s_lem_orig = s_lorig
	      }::l
	    with Ty.TypeClash _ | Deja_vu-> l
	  ) s l
      ) env.fils lsbt_acc

  let add_msymb tbox f t ({sbs=s_t; sty=s_ty} as sg)= 
    try 
      if X.are_equal tbox t (SubstT.find f s_t) = Sig.No then raise Echec;
      sg 
    with Not_found ->  {sg with sbs=SubstT.add f t s_t; sty=s_ty}

  let (-@) l1 l2 =
    match l1, l2 with
      | [], _  -> l2
      | _ , [] -> l1
      | _ -> List.fold_left (fun acc e -> e :: acc) l2 (List.rev l1)

  let rec match_term env tbox ( {sbs=s_t; sty=s_ty;gen=g;goal=b} as sg) pat t =
    Options.exec_thread_yield ();
    Debug.match_term sg t pat;
    let {T.f=f_pat;xs=pats;ty=ty_pat} = T.view pat in
    match f_pat with
      |	Symbols.Var _ -> 
	let sb =
          (try
	     let s_ty = Ty.matching s_ty ty_pat (T.view t).T.ty in
	     let g',b' = infos max (||) t g b env in
	     add_msymb tbox f_pat t 
	       { sg with sbs=s_t; sty=s_ty; gen=g'; goal=b' }
	   with Ty.TypeClash _ -> raise Echec)
        in 
        [sb]
      | _ -> 
        try
          let s_ty = Ty.matching s_ty ty_pat (T.view t).T.ty in
          let cl = if no_Ematching () then [t] else X.class_of tbox t in
          Debug.match_class_of t cl;
          let cl =
	    List.fold_left
	      (fun l t -> 
                let {T.f=f; xs=xs} = T.view t in
	        if Symbols.compare f_pat f = 0 then xs::l else l
	      )[] cl
          in
          let gsb = { sg with sbs = s_t; sty = s_ty } in
          (* pas sur que ce soit correct ici *)
          List.fold_left
            (fun acc xs -> 
              try (match_list env tbox gsb pats xs) -@ acc
              with Echec -> acc
            ) [] cl
        with Ty.TypeClash _ -> raise Echec

  and match_list env tbox sg pats xs = 
    Debug.match_list sg pats xs;
    try 
      List.fold_left2 
        (fun sb_l pat arg -> 
          List.fold_left 
            (fun acc sg -> 
              let aux = match_term env tbox sg pat arg in
              (*match aux with [] -> raise Echec | _  -> BUG !! *)
              List.rev_append aux acc
            ) [] sb_l
        ) [sg] pats xs 
    with Invalid_argument _ -> raise Echec

  let match_one_pat env tbox pat_info pat0 lsbt_acc sg =
    Debug.match_one_pat sg pat0;
    let pat = T.apply_subst (sg.sbs, sg.sty) pat0 in
    let {T.f=f; xs=pats; ty=ty} = T.view pat in
    match f with
      | Symbols.Var _ -> all_terms f ty env pat_info sg lsbt_acc
      | _ -> 
        let {sty=sty; gen=g; goal=b} = sg in
        let f_aux t xs lsbt = 
          Debug.match_one_pat_against sg pat0 t;
	  try
	    let s_ty = Ty.matching sty ty (T.view t).T.ty in
	    let gen, but = infos max (||) t g b env in
            let sg =
              { sg with 
                sty = s_ty; gen = gen; goal = but; 
                s_term_orig = t::sg.s_term_orig }
            in
	    let aux = match_list env tbox sg pats xs in
            List.rev_append aux lsbt
	  with Echec | Ty.TypeClash _ -> lsbt
        in
	try MT.fold f_aux (SubstT.find f env.fils) lsbt_acc
	with Not_found -> lsbt_acc

  let match_pats_modulo env tbox pat_info lsubsts pat = 
    Debug.match_pats_modulo pat lsubsts;
    List.fold_left (match_one_pat env tbox pat_info pat) [] lsubsts
      
  let matching env tbox (pat_info, pats) =
    Debug.matching pats;
    let egs = 
      { sbs = SubstT.empty;
        sty = Ty.esubst; 
        gen = 0; 
	goal = false; 
	s_term_orig = [];
	s_lem_orig = pat_info.trigger_orig;
      } 
    in
    pat_info, List.fold_left (match_pats_modulo env tbox pat_info) [egs] pats

  let query env tbox = 
    Options.exec_timer_start Timers.TMatch;
    let res = List.rev_map (matching env tbox) env.pats in
    Options.exec_timer_pause Timers.TMatch;
    res

end

(*** Instantiation part of the module ***)

module type S = sig
  type t
  type tbox

  val empty : t
  val add_terms : t -> T.Set.t -> F.gformula -> t
  val add_lemma : t -> F.gformula -> Ex.t -> t
  val add_predicate : t -> F.gformula -> t

  type instances = (F.gformula * Ex.t) list

  val m_lemmas :
    t ->
    tbox ->
    (F.t -> F.t -> bool) ->
    instances * instances (* goal_directed, others *)

  val m_predicates :
    t ->
    tbox ->
    (F.t -> F.t -> bool) ->
    instances * instances (* goal_directed, others *)

  (* returns names of used axioms/predicates * unused axioms/predicates *)
  val retrieve_used_context : t -> Ex.t -> string list * string list

end

module Make(X : Theory.S) : S with type tbox = X.t = struct
    
  module EM = EMatch(X) 

  type tbox = X.t
  type instances = (F.gformula * Ex.t) list

  type t = {
    lemmas : (int * Ex.t) MF.t;
    predicates : (int * Ex.t) MF.t;
    matching : EM.t;
  }

  let empty = {
    lemmas = MF.empty ; 
    matching = EM.empty;
    predicates = MF.empty;
  }

  let add_terms env s gf =
    Options.exec_timer_start Timers.TMatch;
    let infos = { 
      term_age = gf.F.age ; 
      term_from_goal    = gf.F.gf ;
      term_from_formula = gf.F.lem ;
      term_from_terms   = gf.F.from_terms
    }
    in
    let env = 
      { env with
        matching = T.Set.fold (EM.add_term infos) s env.matching } in
    Options.exec_timer_pause Timers.TMatch;
    env

  module SST = Set.Make(String)


  let init_with_replay_used acc f =
    if Sys.command (sprintf "[ -e %s ]" f) <> 0 then
      begin
        fprintf fmt 
          "File %s not found! Option -replay-used will be ignored@." f;
        acc
      end
    else
      let cin = open_in f in
      let acc = ref (match acc with None -> SST.empty | Some ss -> ss) in
      begin
        try while true do acc := SST.add (input_line cin) !acc done;
        with End_of_file -> close_in cin
      end;
      Some !acc

  let used = 
    if Options.replay_used_context () then 
      init_with_replay_used None (Options.get_used_context_file ())
    else if Options.replay_all_used_context () then 
      let dir = Filename.dirname (Options.get_used_context_file ()) in
      Array.fold_left
        (fun acc f ->
          let f = sprintf "%s/%s" dir f in
          if (Filename.check_suffix f ".used") then begin
            init_with_replay_used acc f
          end
          else acc
        ) None (Sys.readdir dir)
    else None

  let unused_context f = match used, F.view f with
    | None  , _ -> false 
    | Some s_used, F.Lemma {F.name=s} -> not (s = "" || SST.mem s s_used)
    | _ -> assert false

  let add_lemma env gf dep =
    let {F.f=f;age=age} = gf in
    if (*not (Ex.is_empty dep) ||*) unused_context f then env
    else
      let age , dep = 
        try 
          let age' , dep' = MF.find f env.lemmas in
          min age age' , Ex.union dep dep' 
        with Not_found -> age , dep 
      in
      { env with lemmas=MF.add f (age,dep) env.lemmas }
        
  let add_predicate env gf =
    let {F.f=f;age=age} = gf in
    if unused_context f then env
    else { env with predicates = MF.add f (age,Ex.empty) env.predicates }

  let mtriggers env formulas = 
    MF.fold
      (fun lem (age, dep) env ->
	match F.view lem with
	    F.Lemma {F.triggers = tgs; main = f} -> 
	      List.fold_left 
		(fun env (tg, a) ->
		  let info = 
		    { trigger_age = age ; 
		      trigger_orig = lem ;
		      trigger_formula = f ;
		      trigger_dep = dep ; 
		      trigger_query = a }
		  in
		  { env with 
		    matching = 
		      EM.add_trigger info tg env.matching })
		env tgs
	  | _ -> assert false		 
      )
      formulas env

  let new_facts env tbox selector = 
    List.fold_left
      (fun acc ({trigger_formula=f; trigger_query = guard; 
	         trigger_age=age; trigger_dep=dep; trigger_orig=orig }, 
	        subst_list) ->
        List.fold_left
	  (fun ((gd, ngd) as acc)
	    {sbs = sbs; 
	     sty = sty; 
	     gen = g; 
	     goal = b; 
	     s_term_orig = torig;
	     s_lem_orig = lorig; } ->
              if not (F.equal orig lorig) then begin
                fprintf fmt "orig =%a@.@." F.print orig;
                fprintf fmt "lorig=%a@." F.print lorig;
                assert false
              end;
              let sbs = 
                Symbols.Map.fold
                  (fun k t mp ->
                    let t = 
                      if Options.normalize_instances () ||
                        List.length (X.class_of tbox t) > 4 then 
                        X.term_repr tbox t
                      else t
                    in
                    Symbols.Map.add k t mp
                  )sbs Symbols.Map.empty
              in
              let s = sbs, sty in
	      match guard with
	        | Some a when 
		    X.query (Literal.LT.apply_subst s a) tbox = No ->
                  acc
	        | _ ->
		  let nf = F.apply_subst s f in
		  if selector nf orig then 
		    let p = 
		      { F.f = nf;
		        age = 1+(max g age);
		        mf = true;
		        gf = b;
		        lem = Some lorig;
		        from_terms = torig
		      } 
                    in
                    if b then (* formula's terms are related to the goal *)
                      (p,dep) :: gd, ngd
                    else 
                      gd, (p,dep) :: ngd
                  else acc
	  ) 
	  acc subst_list
      )
      ([], []) (EM.query env.matching tbox)


  let sort_facts =
    let rec size f = match F.view f with
      | F.Unit(f1,f2) -> max (size f1) (size f2)
      | _             -> F.size f
    in
    fun lf ->
      List.fast_sort
        (fun (p1,_) (p2,_) -> 
          let c = size p1.F.f - size p2.F.f in
          if c <> 0 then c
          else F.compare p2.F.f p1.F.f
        ) lf

  let mround env axs tbox selector =
    Options.tool_req 2 "TR-Sat-Mround";
    Options.exec_timer_start Timers.TMatch;
    let env = mtriggers env axs in
    let gd, ngd = new_facts env tbox selector in
    Options.exec_timer_pause Timers.TMatch;
    sort_facts gd, sort_facts ngd

  let m_lemmas env tbox selector = mround env env.lemmas tbox  selector

  let m_predicates env tbox selector = mround env env.predicates tbox selector


  module MI = Map.Make (struct type t = int let compare = compare end)

  let ids_of_context axs mp = 
    MF.fold
      (fun lem (_,ex) mp -> 
        if Ex.is_empty ex then
          match F.view lem with
            | F.Lemma {F.name=name; F.main=f} -> 
              let id = F.id f in
              assert (not (MI.mem id mp));
              MI.add id name mp
            | _ -> assert false
        else mp
      )axs mp
      
  let retrieve_used_context env dep = 
    let ctx = ids_of_context env.lemmas MI.empty in
    let ctx = ids_of_context env.predicates ctx in
    let used, ununsed_mp = 
      SF.fold
        (fun dep ((used, ctx) as acc) ->
          let id = F.id dep in
          try (MI.find id ctx) :: used, MI.remove id ctx
          with Not_found -> acc
        ) (Ex.formulas_of dep) ([], ctx)
    in
    let ununsed = MI.fold (fun _ name acc -> name :: acc) ununsed_mp [] in
    used, ununsed

end
