
module WT = WhyTrans
module WC = WhyCore

open Why3
open Support.Error
open Support.Options
   
open Why3.Ty
open Why3.Term
open WT 
module CP = Call_provers

exception Why3_error of string

let why_error   fi   = raise( Why3_error fi)

let dp = Support.FileInfo.dummyinfo

 let why_debug  fi = message 1 SMT fi


let ps_plus =
  let v = ty_var (create_tvsymbol (Ident.id_fresh "a")) in
  Term.create_psymbol (Ident.id_fresh "infix +") [v; v]

let real_theory : Why3.Theory.theory = Why3.Env.read_theory WhyCore.env ["real"] "Real"
let birel_theory    : WT.theory = WEnv.read_theory WC.env ["birel"]   "Birel"
let bool_theory :  WT.theory = WEnv.read_theory WC.env ["bool"]   "Bool"
let arel_theory    : WT.theory = WEnv.read_theory WC.env ["birel"]   "Arel"

let set_theory:    Why3.Theory.theory = Why3.Env.read_theory WhyCore.env ["set"] "Set"
let test_theory : WT.theory = WEnv.read_theory WC.env ["birel"]   "Test" 
let real_trun_theory : WT.theory = WEnv.read_theory WC.env ["real"] "Truncate"

let get_why3_set th name =
  try Why3.Theory.ns_find_ts th.Why3.Theory.th_export [name]
  with Not_found -> why_error "Primitive %s cannot be mapped to Why3" name

let set_type = get_why3_set set_theory "set"

(* let set_type: Ty.tysymbol =  Why3.Theory.ns_find_ts set_theory.Why3.Theory.th_export ["SetGen"]
 *)
let set_int_type = Ty.ty_app set_type [Ty.ty_int]

           

 (* let array_module : Mlw_module.modul =
  Mlw_module.read_module WC.env ["array"] "Array"

let array_theory : Theory.theory =
  array_module.Mlw_module.mod_theory

let array_type : Mlw_ty.T.itysymbol =
  Mlw_module.ns_find_its array_module.Mlw_module.mod_export ["array"] *)

  

let get_why3_sym th name =
  try Why3.Theory.ns_find_ls th.Why3.Theory.th_export [name]
  with Not_found -> Format.printf "%s" name ;why_error "Primitive %s cannot be mapped to Why3" name

(* let why3_get= get_why3_sym array_theory "get"

let why3_update= get_why3_sym array_theory "set"

let why3_length= get_why3_sym array_theory "length"              *)                                              

(* let why3_array_make= get_why3_sym arel_theory "o"  *)


let why3_rplus  = get_why3_sym real_theory "infix +"

let why3_rsub  = get_why3_sym real_theory "infix -"

(* let why3_aget= get_why3_sym arel_theory "aget"

let why3_aupdate= get_why3_sym arel_theory "aupdate"

let why3_alength= get_why3_sym arel_theory "alength" *)

  
let why3_beq1= get_why3_sym bool_theory "infix ="

let why3_emptyset= get_why3_sym test_theory "emptyset"
let why3_fullset= get_why3_sym test_theory "full"

let why3_rangeset= get_why3_sym test_theory "range"

let why3_unionset= get_why3_sym test_theory "un"


let why3_interset= get_why3_sym test_theory "it"
let why3_diffset= get_why3_sym test_theory "df"

let why3_mem= get_why3_sym test_theory "mem"

let why3_seteq= get_why3_sym test_theory "infix =="

let why3_subset= get_why3_sym test_theory "subset"

let why3_all = get_why3_sym set_theory "all"


            


let rec fmla_rewrite_subst f =
  match f.t_node with
    | Tapp (ls,[{t_node=Tvar vs} as tv;t]) when ls_equal ls ps_equ ->
      begin
      match t.t_node with
      | Tapp (ls', [t';{t_node=Tvar vs'} as tv']) when ls_equal ls'  why3_rplus -> print_string "HERE HERE\n\n";
        t_app_infer ps_equ  [tv'; (t_app_infer why3_rsub [ tv; t'])]
      | _ -> f end
      | _ -> TermTF.t_map (fun t -> t) fmla_rewrite_subst f



let simplify_trivial_quantification_in_goal2 =
  Trans.goal (fun pr f -> [Decl.create_prop_decl Pgoal pr (Simplify_formula.fmla_remove_quant false (fmla_rewrite_subst f))])

let simplify_trivial_quantification_in_goal =
 Trans.rewriteTF (fun t -> Simplify_formula.fmla_simpl t) (fun t -> Simplify_formula.fmla_remove_quant false t )  None
 
(* let z3 : Whyconf.config_prover =
  let fp = Whyconf.parse_filter_prover "Z3"   in
  (** all provers that have the name "Alt-Ergo" *)
  let provers = Whyconf.filter_provers WC.config fp in
  if Whyconf.Mprover.is_empty provers then begin
    Format.eprintf "Prover Z3 not installed or not configured@.";
    exit 0
  end else
    snd (Whyconf.Mprover.max_binding provers) *)


let alt_ergo : Whyconf.config_prover =
  let fp = Whyconf.parse_filter_prover "Alt-Ergo"   in
  (** all provers that have the name "Alt-Ergo" *)
  let provers = Whyconf.filter_provers WC.config fp in
  if Whyconf.Mprover.is_empty provers then begin
    Format.eprintf "Prover Alt-Ergo not installed or not configured@.";
    exit 0
  end else
    snd (Whyconf.Mprover.max_binding provers)

        
(* loading the Alt-Ergo driver *)
let alt_ergo_driver : Driver.driver =
  try
    Whyconf.load_driver WC.main  WC.env alt_ergo.Whyconf.driver []
  with e ->
    Format.eprintf "Failed to load driver for alt-ergo: %a@."
      Exn_printer.exn_printer e;
    exit 1

let splitGoals = ref false

let timelimit = ref 30

let timelimit_st = ref 1
let smt_time =  ref 0.0

let prove_alt_ergo task showDebug =
  let result : Call_provers.prover_result =
    Call_provers.wait_on_call
      (Driver.prove_task ~command:alt_ergo.Whyconf.command ~limit:{Call_provers.empty_limit with limit_time = (!timelimit)} alt_ergo_driver task )              in
  (if showDebug then why_debug dp "@[Alt-Ergo answers %a@]@." Call_provers.print_prover_result result);
  match result.CP.pr_answer with
  | CP.Valid   -> true
  | CP.Invalid -> false
  | CP.Unknown (s,_) -> why_debug dp "UNKNOWN %s"  s ; false
  | _          -> false

let prove_alt_ergo_st task showDebug =
  let result : Call_provers.prover_result =
    Call_provers.wait_on_call
      (Driver.prove_task ~command:alt_ergo.Whyconf.command ~limit:{Call_provers.empty_limit with limit_time = (!timelimit_st)} alt_ergo_driver task )              in
  (if showDebug then why_debug dp "@[Alt-Ergo answers %a@]@." Call_provers.print_prover_result result);
  match result.CP.pr_answer with
  | CP.Valid   -> true
  | CP.Invalid -> false
  | CP.Unknown (s,_) -> why_debug dp "UNKNOWN %s"  s ; false
  | _          -> false




let post cs i =

  let task    = None                                                   in
   why_debug dp  "!*! Why3 term: @[%a@]"  Why3.Pretty.print_term cs  ;
  let goal_id = Decl.create_prsymbol (Ident.id_fresh "ty_goal")        in
  let task    = Task.use_export task real_theory                    in
  let task    = Task.use_export task birel_theory                  in
  let task    = Task.use_export task arel_theory                  in
  let task    = Task.use_export task bool_theory                  in
 (*  let task    = Task.use_export task array_theory                  in *)
  let task = Task.use_export task test_theory in 
  let task = Task.use_export task set_theory in
  let task = Task.use_export task real_trun_theory in

  let task    = Task.add_prop_decl task Decl.Pgoal goal_id cs          in

  let simpl   = simplify_trivial_quantification_in_goal  in
  let task    = Trans.apply simpl task in
 let res = prove_alt_ergo task true in
  if !splitGoals then 
    
    if not (res) then
      let trans_task =  Trans.apply_transform "split_goal_wp" WC.env task in
      why_debug dp "First %i  attempt failed, next will try splitting the goal and proving subgoals: \n" i;
      why_debug dp  "calling solver on subgoals....";
      List.fold_left (fun acc t -> let res = prove_alt_ergo t true in
        	       if not res then
                  let t' = Trans.apply simpl t in
                  let term = (Task.task_goal_fmla t') in
                  why_debug dp  "failed subgoals: @[%a@]" Why3.Pretty.print_term term ;
       	          let sub_res =
                    (prove_alt_ergo t' true) in 
(*                     why_debug dp  "!*! fails cs term: @[%a@]"  Why3.Pretty.print_term ;
 *)                     sub_res
        	else
           res && acc) true trans_task
    else true
  else res

let post_st cs i =

  let task    = None                                                   in
   why_debug dp  "!*! Why3 term: @[%a@]"  Why3.Pretty.print_term cs  ;
  let goal_id = Decl.create_prsymbol (Ident.id_fresh "ty_goal")        in
  let task    = Task.use_export task real_theory                    in
  let task    = Task.use_export task birel_theory                  in
  let task    = Task.use_export task arel_theory                  in
  let task    = Task.use_export task bool_theory                  in
  (* let task    = Task.use_export task array_theory                  in *)
  let task = Task.use_export task test_theory in 
  let task = Task.use_export task set_theory in
  let task = Task.use_export task real_trun_theory in
  
  let task    = Task.add_prop_decl task Decl.Pgoal goal_id cs          in

  let simpl   = simplify_trivial_quantification_in_goal  in
  let task    = Trans.apply simpl task in
 let res = prove_alt_ergo_st task true in
  res



(* let ity = Mlw_ty.ity_app_fresh array_type [Mlw_ty.ity_bool] 
let a = Mlw_ty.create_pvsymbol (Ident.id_fresh "a") ity 
let   vt = T.create_vsymbol (I.id_fresh "i")  (Ty.ty_int) 
let pre =Term.t_app_infer why3_get [Term.t_var a.Mlw_ty.pv_vs;Term.t_var vt]
let pre2 = Term.t_app_infer why3_beq1 [pre; Term.t_bool_true]
let pre1 = Term.t_forall_close [vt] [] pre2
         let pre4 = T.t_binary T.Timplies (T.t_app_infer why3_predicate [Term.t_var a.Mlw_ty.pv_vs]) (pre1)
let pre3 = Term.t_forall_close [a.Mlw_ty.pv_vs] [] pre4  *)




 (*  let const_array =   Term.t_app_infer why3_array_make  []  
     let   vt = T.create_vsymbol (I.id_fresh "i")  (Ty.ty_int)           
  let getfirstidx = Term.t_app_infer why3_aget [const_array; Term.t_var vt ]
  let firstEQtrue =   Term.t_app_infer why3_beq1 [ getfirstidx ; Term.t_bool_true]
  let test = Term.t_forall_close [vt] [] firstEQtrue
  let updatedarray=   Term.t_app_infer why3_aupdate [const_array; why3_int 1; Term.t_bool_false]
  let getfirstidxA = Term.t_app_infer why3_aget [ updatedarray; why3_int 1 ]
  let firstEQtrueA=   Term.t_app_infer why3_beq1 [ getfirstidxA ; Term.t_bool_false]
                    
  

    let createArrayTerm (a:string) =
  let ity = Mlw_ty.ity_app_fresh array_type [Mlw_ty.ity_bool] in
              Mlw_ty.create_pvsymbol (Ident.id_fresh a) ity
let createIdxTerm (i:string) =
    T.create_vsymbol (I.id_fresh i)  (Ty.ty_int)
  
let createGetTerm a i =Term.t_app_infer why3_get [Term.t_var a.Mlw_ty.pv_vs;Term.t_var i]
                     
let checkValueTrue v = Term.t_app_infer why3_beq1 [v; Term.t_bool_true]
                     
let createForall i fml = Term.t_forall_close [i] [] fml

let createImplyPredicate a fml = T.t_true (* T.t_binary T.Timplies (T.t_app_infer why3_predicate [Term.t_var a.Mlw_ty.pv_vs]) (fml) *)

let createExistArray a fml = Term.t_exists_close [a.Mlw_ty.pv_vs] [] fml  *)


                 
let send_smt cs =
  let t = Unix.gettimeofday () in
  (* let vt = T.create_vsymbol (I.id_fresh "v")  (set_int_type)      in
  let var = T.t_var vt in
  let full = T.t_app_infer why3_fullset []  in
  let empty = T.t_app_infer why3_emptyset [] in
  let set2 = T.t_app_infer why3_unionset [ empty; var ] in  
  let eq =  T.t_app_infer why3_seteq [  set2 ; set2  ] in 
  let rg1 = T.t_app_infer why3_rangeset   [why3_int 1; why3_int 3]  in
  let rg2 = T.t_app_infer why3_rangeset  [why3_int 2; why3_int 3] in
  let emptyUrg1 = T.t_app_infer why3_unionset [ rg1; var ] in 
  let emptyUrg2 = T.t_app_infer why3_unionset [ rg2; var ] in 
  let eq2 =  T.t_app_infer why3_subset [  emptyUrg2 ; emptyUrg1  ] in
  let pre1 = Term.t_forall_close [vt] [] eq2 in
 *)
   why_debug dp  "!*! before translate, the cs is @[%a@]"  Print.pp_cs cs ;
  let why3_cs =   WhyTrans.why3_translate_int cs       in

  why_debug dp  "!*! Why3 term: @[%a@]"  Why3.Pretty.print_term why3_cs;
  why_debug dp "!*! -----------------------------------------------";
  
   let res =  post why3_cs 1 in
   why_debug dp "Time for SMT solver:%f\n"  !smt_time; res

  let send_smt_u cs =
  let t = Unix.gettimeofday () in
  (* let vt = T.create_vsymbol (I.id_fresh "v")  (set_int_type)      in
  let var = T.t_var vt in
  let full = T.t_app_infer why3_fullset []  in
  let empty = T.t_app_infer why3_emptyset [] in
  let set2 = T.t_app_infer why3_unionset [ empty; var ] in  
  let eq =  T.t_app_infer why3_seteq [  set2 ; set2  ] in 
  let rg1 = T.t_app_infer why3_rangeset   [why3_int 1; why3_int 3]  in
  let rg2 = T.t_app_infer why3_rangeset  [why3_int 2; why3_int 3] in
  let emptyUrg1 = T.t_app_infer why3_unionset [ rg1; var ] in 
  let emptyUrg2 = T.t_app_infer why3_unionset [ rg2; var ] in 
  let eq2 =  T.t_app_infer why3_subset [  emptyUrg2 ; emptyUrg1  ] in
  let pre1 = Term.t_forall_close [vt] [] eq2 in
 *)
   why_debug dp  "!*! before translate, the cs is @[%a@]"  Print.pp_cs cs ;
  let why3_cs =   WhyTrans.why3_translate cs       in

  why_debug dp  "!*! Why3 term: @[%a@]"  Why3.Pretty.print_term why3_cs;
  why_debug dp "!*! -----------------------------------------------";
  
   let res =  post why3_cs 1 in
   why_debug dp "Time for SMT solver:%f\n" !smt_time ; res 
