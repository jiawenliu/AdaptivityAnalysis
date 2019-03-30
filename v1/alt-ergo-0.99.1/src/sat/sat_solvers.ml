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

module type S = sig
  type t

  exception Sat of t
  exception Unsat of Explanation.t
  exception I_dont_know of t

  (* the empty sat-solver context *)
  val empty : unit -> t
  val empty_with_inst : (Formula.t -> bool) -> t
    
  (* [assume env f] assume a new formula [f] in [env]. Raises Unsat if
     [f] is unsatisfiable in [env] *)
  val assume : t -> Formula.gformula -> t
    
  (* [pred_def env f] assume a new predicate definition [f] in [env]. *)
  val pred_def : t -> Formula.t -> t

  (* [unsat env f size] checks the unsatisfiability of [f] in
     [env]. Raises I_dont_know when the proof tree's height reaches
     [size]. Raises Sat if [f] is satisfiable in [env] *)
  val unsat : t -> Formula.gformula -> Explanation.t

  val print_model : header:bool -> Format.formatter -> t -> unit

  val start : unit -> unit
  val stop : unit -> int64

  (* returns names of used axioms/predicates * unused axioms/predicates *)
  val retrieve_used_context : t -> Explanation.t -> string list * string list
end

(*** Implementation of Dfs_sat ***)
module Dfs_sat : S = struct

  module Th = Theory.Main
  open Options
  open Format
  open Sig
  module A = Literal
  module F = Formula
  module Inst = Matching.Make(Th) 
  module SF = F.Set
  module MF = F.Map
  module Ex = Explanation

  let steps = ref 0L

  module H = Hashtbl.Make(Formula)

  type t = { 
    gamma : (Ex.t * F.gformula) MF.t; 
    delta : (F.gformula * F.gformula * Ex.t) list;
    tbox : Th.t;
    inst : Inst.t;
    add_inst: Formula.t -> bool;
  }

  exception Sat of t
  exception Unsat of Ex.t
  exception I_dont_know of t
  exception IUnsat of Ex.t * Term.Set.t list


  (*BISECT-IGNORE-BEGIN*)
  module Debug = struct

    let pred_def f = 
      if debug_sat () then
        eprintf "[sat] I assume a predicate: %a@.@." F.print f

    let unsat_rec dep =
      if debug_sat () then fprintf fmt "unsat_rec : %a@." Ex.print dep

    let assume gf dep = 
      let {F.f=f;age=age;lem=lem;mf=mf;from_terms=terms} = gf in
      if debug_sat () then
        begin
	  (match F.view f with
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
	        Symbols.print lvar Term.print lterm F.print lf);
	  fprintf fmt " with explanations : %a@." Explanation.print dep
        end

    let unsat () = 
      if debug_sat () then fprintf fmt "[sat] unsat@."

    let decide f = 
      if debug_sat () then 
        fprintf fmt "[sat] I decide on %a@." F.print f

    let backtracking f = 
      if debug_sat () then 
        fprintf fmt "[sat] I backtrack on %a@." F.print f

    let backjumping f = 
      if debug_sat () then 
        fprintf fmt "[sat] I don't consider the case %a@." F.print f
          
    let elim _ _ = 
      if debug_sat () && verbose () then fprintf fmt "[sat] elim@."

    let red _ _ = 
      if debug_sat () && verbose () then fprintf fmt "[sat] red@."

    let delta d = 
      if debug_sat () && verbose () && false then begin
        fprintf fmt "[sat] - Delta ---------------------@.";
        List.iter (fun (f1, f2, ex) ->
	  fprintf fmt "(%a or %a), %a@." 
            F.print f1.F.f F.print f2.F.f Ex.print ex) d;
        fprintf fmt "[sat] --------------------- Delta -@."
      end
        
    let gamma g =
      if debug_sat () && verbose () && false then begin
        fprintf fmt "[sat] - GAMMA ---------------------@.";
        MF.iter (fun f (ex, _) ->
	  fprintf fmt  "%a \t->\t%a@." F.print f Ex.print ex) g;
        fprintf fmt  "[sat] --------------------- GAMMA -@."
      end
        
    let bottom classes =
      if bottom_classes () then
        printf "bottom:%a\n@." Term.print_tagged_classes classes      
          
    let inconsistent expl =
      if debug_sat () then fprintf fmt "inconsistent %a@." Ex.print expl

  end
  (*BISECT-IGNORE-END*)

  let selector env f orig =
    not (MF.mem f env.gamma)
    && begin match F.view orig with
      | F.Lemma _ -> env.add_inst orig
      | _ -> true
    end

  let is_literal f = match F.view f with F.Literal _ -> true | _ -> false

  let extract_prop_model t = 
    let s = ref SF.empty in
    MF.iter 
      (fun f _ -> 
        if (complete_model () && is_literal f) || F.is_in_model f then
	  s := SF.add f !s
      ) 
      t.gamma;
    !s

  let print_prop_model fmt s =
    SF.iter (fprintf fmt "\n %a" F.print) s

  let print_model ~header fmt t =
    Format.print_flush ();
    if header then fprintf fmt "\nModel\n@.";
    let pm = extract_prop_model t in
    if not (SF.is_empty pm) then begin
      fprintf fmt "Propositional:";
      print_prop_model fmt pm;
      fprintf fmt "\n@.";
    end;
    Th.print_model fmt t.tbox


  let _ =
    if not (model ()) then
      try
        Sys.set_signal Sys.sigalrm
	  (Sys.Signal_handle (fun _ -> Options.exec_timeout ()))
      with Invalid_argument _ -> ()

  let refresh_model_handler =
    if model () then
      fun t ->
        try
	  Sys.set_signal Sys.sigalrm
	    (Sys.Signal_handle (fun _ ->
	      printf "%a@." (print_model ~header:true) t;
	      Options.exec_timeout ()))
        with Invalid_argument _ -> ()
    else fun _ -> ()

  (* sat-solver *)

  let elim {F.f=f} env = 
    if MF.mem f env.gamma then
      begin
        Options.tool_req 2 "TR-Sat-Bcp-Elim-1";
        true
      end
    else
      let el = match F.view f with 
        | F.Literal a -> Th.query a env.tbox <> No
        | _ -> false
      in
      if el then 
        Options.tool_req 2 "TR-Sat-Assume-Let";
      el


  let red {F.f=f} env = 
    let nf = F.mk_not f in
    try 
      let r = Yes (fst (MF.find nf env.gamma), Th.cl_extract env.tbox) in
      Options.tool_req 2 "TR-Sat-Bcp-Red-1";
      r
    with Not_found -> 
      let r = match F.view nf with
        |	F.Literal a -> Th.query a env.tbox
        | _ -> No
      in
      begin match r with 
        |	Yes _ -> Options.tool_req 2 "TR-Sat-Bcp-Red-2"
        | No -> ()
      end;
      r
        
  let pred_def env f = 
    let ff =
      { F.f = f;
        age = 0;
        lem = None;
        mf = false;
        gf = false;
        from_terms = []
      } 
    in
    Debug.pred_def f;
    {env with inst = Inst.add_predicate env.inst ff}


  let add_dep f dep =
    match F.view f with 
      | F.Literal _ when proof () -> 
        if not (Ex.mem (Ex.Bj f) dep) then
	  Ex.union (Ex.singleton (Ex.Dep f)) dep
        else dep
      | F.Clause _ when proof () -> 
        Ex.union (Ex.singleton (Ex.Dep f)) dep
      | _ -> dep
        

  let rec add_dep_of_formula f dep =
    let dep = add_dep f dep in
    match F.view f with 
      | F.Unit (f1, f2) when proof () ->
        add_dep_of_formula f2 (add_dep_of_formula f1 dep)
      | _ -> dep

  let rec asm_aux acc list =
    List.fold_left
      (fun (env, bcp) ({F.f=f} as ff ,dep) ->
        refresh_model_handler env;
        Options.exec_thread_yield ();
        let dep = add_dep f dep in
        let dep_gamma = add_dep_of_formula f dep in
        (try 
           Debug.gamma env.gamma;
           let ex_nf, _ = MF.find (F.mk_not f) env.gamma in
           Options.tool_req 2 "TR-Sat-Conflict-1";
           raise (IUnsat (Ex.union dep_gamma ex_nf, Th.cl_extract env.tbox))
         with Not_found -> ());
        if MF.mem f env.gamma then begin
	  Options.tool_req 2 "TR-Sat-Remove";
	  env, bcp
        end
        else 
	  let env =
	    if ff.F.mf && greedy () then 
              { env with inst=Inst.add_terms env.inst (F.terms f) ff }
            else env
          in
	  let env = { env with gamma = MF.add f (dep_gamma, ff) env.gamma } in
	  Debug.assume ff dep;
	  match F.view f with
	    | F.Unit (f1, f2) ->
	      Options.tool_req 2 "TR-Sat-Assume-U";
	      asm_aux (env, bcp) [{ff with F.f=f1},dep ; {ff with F.f=f2},dep]
	    | F.Clause(f1,f2) -> 
	      Options.tool_req 2 "TR-Sat-Assume-C";
	      let p1 = {ff with F.f=f1} in
	      let p2 = {ff with F.f=f2} in
              let p1, p2 = 
                if F.size f1 <= F.size f2 then p1, p2 else p2, p1 in
	      {env with delta = (p1,p2,dep)::env.delta}, true

	    | F.Lemma l ->
	      Options.tool_req 2 "TR-Sat-Assume-Ax";
              {env with inst = Inst.add_lemma env.inst ff dep}, bcp

	    | F.Literal a ->
	      Options.tool_req 2 "TR-Sat-Assume-Lit";
	      let env = 
	        if ff.F.mf then 
                  {env with inst=Inst.add_terms env.inst (A.LT.terms_of a) ff}
	        else env 
	      in
	      let tbox, new_terms, cpt = Th.assume a dep env.tbox in
	      let inst = Inst.add_terms env.inst new_terms ff in
	      steps := Int64.add (Int64.of_int cpt) !steps;
	      if steps_bound () <> -1 
	        && Int64.compare !steps (Int64.of_int (steps_bound ())) > 0 then 
	        begin 
		  printf "Steps limit reached: %Ld@." !steps;
		  exit 1
	        end;
	      let env = { env with tbox = tbox; inst = inst } in
	      env, true

	    | F.Skolem{F.sko_subst=sigma; sko_f=f} -> 
	      Options.tool_req 2 "TR-Sat-Assume-Sko";
	      let f' = F.apply_subst sigma f in
	      asm_aux (env, bcp) [{ff with F.f=f'},dep]

            | F.Let {F.let_var=lvar; let_term=lterm; let_subst=s; let_f=lf} ->
	      Options.tool_req 2 "TR-Sat-Assume-Let";
              let f' = F.apply_subst s lf in
	      let id = F.id f' in
              let v = Symbols.Map.find lvar (fst s) in
              asm_aux (env, bcp)
                [{ff with F.f=F.mk_lit (A.LT.mk_eq v lterm) id}, dep;
                 {ff with F.f=f'}, dep]
      ) acc list

  and assume env list = 
    try
      let env, do_bcp = asm_aux (env, false) list in
      if do_bcp then bcp env else {env with delta = List.rev env.delta}
    with Exception.Inconsistent (expl, classes) -> 
      Debug.inconsistent expl;
      Options.tool_req 2 "TR-Sat-Conflict-2";
      raise (IUnsat (expl, classes))
        
  and bcp env =
    let cl , u = 
      List.fold_left 
        (fun (cl,u) ((f1,f2,d) as fd) -> 
          Debug.elim f1 f2;
	  if elim f1 env || elim f2 env  then (cl,u)
	  else 
            (Debug.red f1 f2;
	     match red f1 env with
	       | Yes (d1, c1) -> begin
	         match red f2 env with
		   | Yes (d2, c2) -> 
		     let expl = Ex.union (Ex.union d d1) d2 in
		     raise (Exception.Inconsistent (expl, c1@c2))
		   | No -> (cl,(f2,Ex.union d d1)::u)
	       end
	       | No -> 
	         match red f2 env with
		     Yes (d2, _) -> (cl,(f1,Ex.union d d2)::u)
		   | No -> fd::cl , u)
        ) ([],[]) env.delta
    in
    assume {env with delta=cl} u

  let rec unsat_rec env fg = 
    try back_tracking (assume env [fg])
    with IUnsat (d, classes) ->
      Debug.bottom classes;
      Debug.unsat (); 
      d

  and try_greedy env =
    if greedy () then raise (Sat env);
    let gre_inst =
      MF.fold
        (fun f (_,gf) inst -> Inst.add_terms inst (F.terms f) gf)
        env.gamma env.inst
    in
    let gd2, ngd2 = Inst.m_predicates gre_inst env.tbox (selector env) in
    let l2 = List.rev_append (List.rev gd2) ngd2 in
    let env = assume env l2 in

    let gd1, ngd1 = Inst.m_lemmas gre_inst env.tbox (selector env) in

    let l1 = List.rev_append (List.rev gd1) ngd1 in
    let env = assume env l1 in
    begin match l1, l2 with
      | [], [] ->
	if all_models () then
	  begin
	    let m = extract_prop_model env in
	    Format.printf "--- SAT ---\n";
	    Format.printf "%a@." print_prop_model m;
	    raise (IUnsat (Ex.make_deps m, []))
	  end;
        raise (Sat env)
      | l1, l2 -> back_tracking env
    end

  and back_tracking env = match env.delta with
    | [] -> 
      let gd2, ngd2 = Inst.m_predicates env.inst env.tbox (selector env) in 
      let l2 = List.rev_append (List.rev gd2) ngd2 in
      let env = assume env l2 in

      let gd1, ngd1 = Inst.m_lemmas env.inst env.tbox (selector env) in 

      let l1 = List.rev_append (List.rev gd1) ngd1 in
      let env = assume env l1 in
      begin match l1, l2 with
        | [], [] ->
	  if all_models () then 
	    begin
	      let m = extract_prop_model env in
	      Format.printf "--- SAT ---\n";
	      Format.printf "%a@." print_prop_model m;
	      raise (IUnsat (Ex.make_deps m, []))
	    end;
          try_greedy env
        | l1, l2 -> back_tracking env
      end

    | ({F.f=f} as a,b,d)::l ->
      Debug.decide f;
      let dep = unsat_rec {env with delta=l} (a,Ex.singleton (Ex.Bj f)) in
      Debug.unsat_rec dep;
      try
        let dep' = Ex.remove (Ex.Bj f) dep in
        Debug.backtracking (F.mk_not f);
        Options.tool_req 2 "TR-Sat-Decide";
        unsat_rec
	  (assume {env with delta=l} [b, Ex.union d dep'])
	  ({a with F.f=F.mk_not f},dep')
      with Not_found -> 
        Debug.backjumping (F.mk_not f);
        Options.tool_req 2 "TR-Sat-Backjumping";
        dep

	  
  let unsat env gf =
    try
      let env = assume env [gf, Ex.empty] in
      let env = 
        if not gf.F.mf then env
        else {env with inst=Inst.add_terms env.inst (F.terms gf.F.f) gf}
      in
      let gd, ngd = Inst.m_predicates env.inst env.tbox (selector env) in
      let l = List.rev_append (List.rev gd) ngd in
      let env = assume env l in

      (* goal directed for lemmas *) 
      let gd, _ = Inst.m_lemmas env.inst env.tbox (selector env) in
      let env = assume env gd in
      
      back_tracking env
    with IUnsat (dep, classes) ->
      Debug.bottom classes;
      Debug.unsat ();
      dep

  let assume env fg = 
    try assume env [fg,Ex.empty]
    with IUnsat (d, classes) ->
      Debug.bottom classes;
      raise (Unsat d)

  let unsat env fg = 
    if profiling() then
      try 
        Options.exec_timer_start Timers.TSat;
        let env = unsat env fg in
        Options.exec_timer_pause Timers.TSat;
        env
      with e -> 
        Options.exec_timer_pause Timers.TSat;
        raise e
    else unsat env fg

  let assume env fg = 
    if profiling() then
      try 
        Options.exec_timer_start Timers.TSat;
        let env = assume env fg in
        Options.exec_timer_pause Timers.TSat;
        env
      with e -> 
        Options.exec_timer_pause Timers.TSat;
        raise e
    else assume env fg

  let empty () = { 
    gamma = MF.empty;
    delta = [] ;
    tbox = Th.empty (); 
    inst = Inst.empty;
    add_inst = fun _ -> true;
  } 

  let empty_with_inst add_inst =
    { (empty ()) with add_inst = add_inst }

  let start () = steps := 0L
  let stop () = !steps

  let retrieve_used_context env dep = 
    Inst.retrieve_used_context env.inst dep
end

let current = ref (module Dfs_sat : S)

let initialized = ref false
  
let set_current sat = current := sat
  
let load_from path = 
  if Options.debug_sat () then 
    Format.eprintf "I'll try to load the SAT-solver in %s@." path;
  try 
    Dynlink.loadfile path;
    if Options.debug_sat () then Format.eprintf "success !";
    None
  with Dynlink.Error err -> Some err

let load_current_sat () =
  match Options.sat_plugin () with
    | "" ->
      if Options.debug_sat () then Format.eprintf "I'll use Dfs-SAT-solver@."
        
    | given_path -> 
      match load_from given_path with
      | None -> ()
      | Some error1 -> 
        let abs_path = Version.pluginsdir ^ "/" ^ given_path in
        match load_from abs_path with
        | None -> ()
        | Some error2 -> 
          Format.eprintf "Loading the SAT-solver %s@." given_path;
          Format.eprintf "Failure: %s@." (Dynlink.error_message error1);
          Format.eprintf "Loading the SAT-solver %s@." abs_path;
          Format.eprintf "Failure: %s@." (Dynlink.error_message error2);
          exit 1

let get_current () = 
  if not !initialized then
    begin
      load_current_sat ();
      initialized := true;
    end;
  !current
