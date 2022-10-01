module WEnv = Why3.Env
module WT   = Why3.Theory
module WP   = Why3.Pretty

module WC   = WhyCore

module I  = Why3.Ident
module T  = Why3.Term
module D  = Why3.Decl
module Ty = Why3.Ty

open Why3
open Syntax
open IndexSyntax
open Constr
open Format
open Support.Error

module H  = Hashtbl


exception Why3_error of string


exception Elim_Solve_Fails

exception Elim_Solve_Leq_Fails
exception Elim_Switch
exception UnknownWhy3Term
 
let dp = Support.FileInfo.dummyinfo

let why_error   fi   = error_msg Support.Options.SMT fi
let why_debug  fi = message 4 SMT fi
let dp = Support.FileInfo.dummyinfo
(* let why_debug   fi   = Format.fprintf Format.std_formatter  fi *)

let real_theory     : WT.theory = WEnv.read_theory WC.env ["real"] "Real"
let real_log_theory : WT.theory = WEnv.read_theory WC.env ["real"] "ExpLog"
let real_pow_theory : WT.theory = WEnv.read_theory WC.env ["real"] "PowerReal"
let real_min_theory : WT.theory = WEnv.read_theory WC.env ["real"] "MinMax"
let real_trun_theory : WT.theory = WEnv.read_theory WC.env ["real"] "Truncate"
let sum_theory      : WT.theory = WEnv.read_theory WC.env ["birel"] "Sum"
let birel_theory    : WT.theory = WEnv.read_theory WC.env ["birel"]   "Birel"
let int_theory    : WT.theory = WEnv.read_theory WC.env ["int"]   "Int"

let bool_theory :  WT.theory = WEnv.read_theory WC.env ["bool"]   "Bool"
let arel_theory    : WT.theory = WEnv.read_theory WC.env ["birel"]   "Arel"

let set_theory:    WT.theory = Why3.Env.read_theory WC.env ["set"] "Set"
let test_theory : WT.theory = WEnv.read_theory WC.env ["birel"]   "Test" 
let int_min_theory : WT.theory = WEnv.read_theory WC.env ["int"] "MinMax"

(* let array_module : Mlw_module.modul =
  Mlw_module.read_module WC.env ["array"] "Array"

let array_theory : Theory.theory =
  array_module.Mlw_module.mod_theory *)
(* let array_module : Mlw_module.modul =
  Mlw_module.read_module WC.env ["array"] "Array"

let array_theory : Theory.theory =
  array_module.Mlw_module.mod_theory

let array_type : Mlw_ty.T.itysymbol =
  Mlw_module.ns_find_its array_module.Mlw_module.mod_export ["array"] *)

let get_why3_set th name =
  try Why3.Theory.ns_find_ts th.Why3.Theory.th_export [name]
  with Not_found -> why_error dp "Primitive %s cannot be mapped to Why3" name

let set_type = get_why3_set set_theory "set"

(* let set_type: Ty.tysymbol =  Why3.Theory.ns_find_ts set_theory.Why3.Theory.th_export ["SetGen"]
 *)
let set_int_type = Ty.ty_app set_type [Ty.ty_int]

 (* let generate_array_var v = 
  let ity = Mlw_ty.ity_app_fresh array_type [Mlw_ty.ity_bool] in 
   let a = Mlw_ty.create_pvsymbol (Ident.id_fresh v) ity in
        a.Mlw_ty.pv_vs
 *)


 let generate_set_var v = 
   T.create_vsymbol (I.id_fresh "v")  (set_int_type) 

let get_why3_sym th name =
  try WT.ns_find_ls th.WT.th_export [name]
  with Not_found -> why_error dp "Primitive %s cannot be mapped to Why3" name



let why3_eq = get_why3_sym real_theory "infix ="
let why3_leq = get_why3_sym real_theory "infix <="
let why3_lt = get_why3_sym real_theory "infix <"

let why3_radd   = get_why3_sym real_theory "infix +"
let why3_rsub   = get_why3_sym real_theory "infix -"
let why3_rmult  = get_why3_sym real_theory "infix *"
let why3_rdiv   = get_why3_sym real_theory "infix /"
let why3_rlog   = get_why3_sym real_log_theory "log2"
let why3_rpow   =  get_why3_sym real_pow_theory "pow"
let why3_rmin   = get_why3_sym real_min_theory "min"
let why3_rceil  = get_why3_sym birel_theory "cl"
let why3_rfloor = get_why3_sym birel_theory "fl"
let why3_lin_f  = get_why3_sym birel_theory "lin_f"
let why3_con_f  = get_why3_sym birel_theory "con_f"
let why3_rmin_pow  = get_why3_sym birel_theory "min_pow"
let why3_rsum   = get_why3_sym sum_theory "sum"

let why3_leq_int =  get_why3_sym int_theory "infix <="
let why3_lt_int =  get_why3_sym int_theory "infix <"
let why3_eq_int =  get_why3_sym int_theory "infix ="
let why3_add_int   = get_why3_sym int_theory "infix +"
let why3_sub_int   = get_why3_sym int_theory "infix -"
let why3_mult_int  = get_why3_sym int_theory "infix *"

(* let why3_aget= get_why3_sym arel_theory "aget"

let why3_aupdate= get_why3_sym arel_theory "aupdate"

let why3_alength= get_why3_sym arel_theory "alength" *)

(* let why3_get= get_why3_sym array_theory "get"

let why3_update= get_why3_sym array_theory "set"

let why3_length= get_why3_sym array_theory "length"     *)                                                       

(* let why3_array_make= get_why3_sym arel_theory "o" 
 *)

let why3_emptyset= get_why3_sym test_theory "emptyset"
let why3_fullset= get_why3_sym test_theory "full"

let why3_rangeset= get_why3_sym test_theory "range"

let why3_unionset= get_why3_sym test_theory "un"


let why3_interset= get_why3_sym test_theory "it"
let why3_diffset= get_why3_sym test_theory "df"

let why3_mem= get_why3_sym test_theory "mem"

let why3_seteq= get_why3_sym test_theory "infix =="

let why3_subset= get_why3_sym test_theory "subset"
let why3_abs = get_why3_sym test_theory "cardinal"
let why3_min_elt = get_why3_sym test_theory "min_elt"
let why3_max_elt = get_why3_sym test_theory "max_elt"

let why3_fromint  = get_why3_sym birel_theory "fromInt"


let why3_all = get_why3_sym set_theory "all"


  
let why3_beq1= get_why3_sym bool_theory "infix ="

let why3_ceil   = get_why3_sym real_trun_theory "ceil"

let why3_intmax = get_why3_sym test_theory "max"
let why3_intmin = get_why3_sym test_theory "min"


(* We need to keep track of the Why3 variable mapping *)
let vmap : ((string, T.vsymbol) H.t) ref = ref (H.create 256)
let vmap2 : ((string, T.vsymbol) H.t) ref = ref (H.create 256)


(* We need to keep track of the Why3 index function mapping used in summations *)
let lmap : ((string, T.lsymbol) H.t) ref = ref (H.create 8)

let rec get_why3_var_r v =
   if H.mem !vmap v then
      H.find !vmap v
   else
    let vt = T.create_vsymbol (I.id_fresh v)  (Ty.ty_real)
    in H.add !vmap v  vt; get_why3_var_r v

let rec get_why3_var v s =
   if H.mem !vmap v then
      H.find !vmap v
   else
     if (s = Arr) then 
          let vt = generate_set_var v in 
           H.add !vmap v  vt; get_why3_var v s
      else 
      begin
        let vt = T.create_vsymbol (I.id_fresh v)  (Ty.ty_real)
        in H.add !vmap v  vt; get_why3_var v s
      end  

let rec get_why3_var1 v s =

   why_debug dp "!*! get_var_mix: @[%a, %s @]" Print.pp_sort s v;  
   if H.mem !vmap v then
      H.find !vmap v
   else
     if (s = Arr) then 
          let vt = generate_set_var v in 
           H.add !vmap v  vt; get_why3_var1 v s
      else 
      begin
        let vt = T.create_vsymbol (I.id_fresh v)  (Ty.ty_real)
        in H.add !vmap v  vt; get_why3_var1 v s
      end  

  let rec get_why3_var2 v s =

   why_debug dp "!*! get_var_mix2: @[%a, %s @]" Print.pp_sort s v;  
   if H.mem !vmap2 v then
      H.find !vmap2 v
   else
     if (s = Arr) then 
          let vt = generate_set_var v in 
           H.add !vmap2 v  vt; get_why3_var2 v s
      else 
      begin
        let vt = T.create_vsymbol (I.id_fresh v)  (Ty.ty_real)
        in H.add !vmap2 v  vt; get_why3_var2 v s
      end  

let rec get_why3_var_mix v s =

   why_debug dp "!*! get_var_mix: @[%a, %s @]" Print.pp_sort s v;  
   if H.mem !vmap v then
      H.find !vmap v
   else
     if (s = Arr) then 
          let vt = generate_set_var v in 
           H.add !vmap v  vt; get_why3_var_mix v s
      else 
      begin
        let vt = T.create_vsymbol (I.id_fresh v)  (Ty.ty_int)
        in H.add !vmap v  vt; get_why3_var_mix v s
      end  

  let rec get_why3_var_mix2 v s =

   why_debug dp "!*! get_var_mix2: @[%a, %s @]" Print.pp_sort s v;  
   if H.mem !vmap2 v then
      H.find !vmap2 v
   else
     if (s = Arr) then 
          let vt = generate_set_var v in 
           H.add !vmap2 v  vt; get_why3_var_mix2 v s
      else 
      begin
        let vt = T.create_vsymbol (I.id_fresh v)  (Ty.ty_int)
        in H.add !vmap2 v  vt; get_why3_var_mix2 v s
      end  
    

let why3_int s   = T.t_nat_const s



let why3_float f =
  let (f,i) = modf f                                   in
  let is    = Printf.sprintf "%.0f" i                  in
  if i < 0.0 then
    let is_u  = String.sub is 1 (String.length is - 1) in
    let fs    = String.sub (Printf.sprintf "%.3f" f) 3 3 in
    let tzero = T.t_const (  Why3.Number.ConstReal 
                { rc_negative = false ;
                          rc_abs = Why3.Number.real_const_dec "0" "0" None }   ) Ty.ty_real
         in
    let tpos = T.t_const (  Why3.Number.ConstReal 
                { rc_negative = false ;
                          rc_abs = Why3.Number.real_const_dec is_u fs None }   ) Ty.ty_real
     in
    T.t_app_infer why3_rsub [tzero; tpos]
  else
  let fs    = String.sub (Printf.sprintf "%.3f" f) 2 3 in
  T.t_const (  Why3.Number.ConstReal 
                { rc_negative = false ;
                          rc_abs = Why3.Number.real_const_dec is fs None }   ) Ty.ty_real


let why3_fin f = why3_float f

(* let rec  why_array  l =
    match l with 
    | [] -> T.t_app_infer why3_array_make  []  
    | h :: t -> T.t_app_infer why3_aupdate [ why_array t;   why3_int h ; T.t_bool_false] *)

let why3_real2int t = 
   T.t_app_infer why3_ceil [ t]

let rec why3_i_term s =
  why_debug dp "!*! why3_iterm @[s1:%a @]@." Print.pp_iterm s ;
  match s with
  | IVar v         -> 
   why_debug dp "!*! IVar: @[%a@]" Print.pp_vinfo v; 
  begin
    match v.v_type with
    | BiIVar ->
    begin
    if H.mem !vmap v.v_name 
    then let w_v = T.t_var (H.find !vmap v.v_name) in w_v
    else why_error dp "1 Can't find var %a already declared" Print.pp_vinfo v
    end
    | _ -> begin
      if H.mem !vmap2 v.v_name 
    then let w_v = T.t_var (H.find !vmap2 v.v_name) in w_v
    else why_error dp " 2 Can't find var %a already declared" Print.pp_vinfo v
    end
  end
  (* if H.mem !vmap v.v_name 
    then let w_v = T.t_var (H.find !vmap v.v_name) in w_v
    else why_error dp "Can't find var %a already declared" Print.pp_vinfo v *)
  | IConst c       -> why3_fin c
  | IZero          ->  why3_fin 0.0
  | ISucc s        ->  let w_s = why3_i_term s in
    T.t_app_infer why3_radd [why3_fin 1.0; w_s]
  | IAdd (s1,s2)   ->  
    let w_s1 = why3_i_term s1 in
    let w_s2 = why3_i_term s2 in
    T.t_app_infer why3_radd [w_s1; w_s2]
  | IMinus (s1,s2) ->  
    let w_s1 = why3_i_term s1 in
    let w_s2 = why3_i_term s2 in
    T.t_app_infer why3_rsub [w_s1; w_s2]
  | IMult (s1,s2)  ->  
    let w_s1 = why3_i_term s1 in
    why_debug dp "!*! IMult @[s1:%a,s2:%a @]@." Print.pp_iterm s1 Print.pp_iterm s2;
    let w_s2 = why3_i_term s2 in
    why_debug dp "!*! After IMult @[s1:%a,s2:%a @]@." Print.pp_iterm s1 Print.pp_iterm s2;
    T.t_app_infer why3_rmult [w_s1; w_s2]
  | IDiv (s1,s2)  ->  
    let w_s1 = why3_i_term s1 in
    let w_s2 = why3_i_term s2 in
    T.t_app_infer why3_rdiv [w_s1; w_s2]
  | ILog s        ->  let w_s = why3_i_term s in
    T.t_app_infer why3_rlog [w_s]
  | ICeil s       ->  let w_s = why3_i_term s in
    T.t_app_infer why3_rceil [w_s]
  | IFloor s     ->  let w_s = why3_i_term s in
    T.t_app_infer why3_rfloor [w_s]
  | IPow (s1,s2)  ->  
    let w_s1 = why3_i_term s1 in
    let w_s2 = why3_i_term s2 in
    T.t_app_infer why3_rpow [w_s1; w_s2]
  | IMin (s1,s2)  ->  
    let w_s1 = why3_i_term s1 in
    let w_s2 = why3_i_term s2 in
    T.t_app_infer why3_rmin [w_s1; w_s2]
  | IMinPowLin (s1, s2) ->
    let w_s1 = why3_i_term s1 in
    let w_s2 = why3_i_term s2 in
    T.t_func_app 
      (T.t_func_app
         (T.t_func_app (T.t_app_infer  why3_rmin_pow []) w_s1) w_s2) 
      (T.t_app_infer why3_lin_f [])
  | IMinPowCon (s1, s2) ->
    let w_s1 = why3_i_term s1 in
    let w_s2 = why3_i_term s2 in
    T.t_func_app 
      (T.t_func_app
         (T.t_func_app (T.t_app_infer  why3_rmin_pow []) w_s1) w_s2) 
      (T.t_app_infer why3_con_f [])
  | ISum (s, s1,s2)  ->
    let w_s1 = why3_i_term s1 in
    let w_s2 = why3_i_term s2 in
    let w_s = why3_i_term s in
    T.t_app_infer why3_rsum [w_s1; w_s2; w_s]
 
  | IBetaABS (beta) -> 
    why_debug dp "!*! IBETAABS @[b:%a @]@." Print.pp_beta beta;
      let arr =  beta_to_why3_r beta in 
        let abs = T.t_app_infer why3_abs [arr] in 
          T.t_app_infer why3_fromint [abs] 
  | IBetaMin beta -> 
       why_debug dp "!*! IBETAMIN @[b:%a @]@." Print.pp_beta beta;
      let arr =  beta_to_why3_r beta in 
       why_debug dp "!*! IBETAMIN2 @[b:%a @]@." Why3.Pretty.print_term  arr;
        let min = T.t_app_infer why3_min_elt [arr] in 
          T.t_app_infer why3_fromint [min] 
  | IMinimal (i1, i2) -> 
          let w_1 = why3_i_term i1 in 
          let w_2 = why3_i_term i2 in
           let w_3 =  why3_real2int w_1 in 
           let w_4 = why3_real2int w_2 in 
          let min =  T.t_app_infer why3_intmin [w_3;w_4] in 
           why_debug dp "!*! IBETAMIN2 @[b:%a @]@." Why3.Pretty.print_term  min;
          T.t_app_infer why3_fromint [min] 
   | IMaximal (i1, i2) -> 
          let w_1 = why3_i_term i1 in 
          let w_2 = why3_i_term i2 in
           let w_3 =  why3_real2int w_1 in 
           let w_4 = why3_real2int w_2 in 
          let min =  T.t_app_infer why3_intmax [w_3;w_4] in 
          T.t_app_infer why3_fromint [min] 
  | IInfty -> why3_float 0.0
  | _ -> why_error dp "Unknown why3 index term\n"

 and  beta_to_why3_r beta  =
    match beta with
    | BIO ->  T.t_app_infer why3_fullset [] 
    | BIE ->  T.t_app_infer why3_emptyset [] 
    | BVar v ->   
   (*  begin
    match v.v_type with
    | BiIVar -> *)
     T.t_var (get_why3_var v.v_name Arr)
   (*  begin
    if H.mem !vmap v.v_name 
    then let w_v = T.t_var (H.find !vmap v.v_name) in w_v
    else why_error dp "1 Can't find var %a already declared" Print.pp_vinfo v
    end
    | _ -> begin
      if H.mem !vmap2 v.v_name 
    then let w_v = T.t_var (H.find !vmap2 v.v_name) in w_v
    else why_error dp " 2 Can't find var %a already declared" Print.pp_vinfo v
    end
  end *)
    (* T.t_var (get_why3_var v.v_name Arr) *)
    | BRange (i_1,i_2) -> 
        let w_1 =  why3_i_term i_1 in 
         let w_2 =  why3_i_term i_2 in
           let w_3 =  why3_real2int w_1 in 
           let w_4 = why3_real2int w_2 in 
           why_debug dp "!*! BRange2 @[b:%a @]@." Why3.Pretty.print_term  w_4;
            T.t_app_infer why3_rangeset [w_3 ; w_4] 
    | BPos i -> 
        let w =  why3_i_term i in
        let w_1 =  why3_real2int w in  
            T.t_app_infer why3_rangeset [w_1 ; w_1] 
    | BUnion (b_1, b_2 )  ->
       why_debug dp "!*! BUnion @[b1:%a,b2:%a @]@." Print.pp_beta b_1 Print.pp_beta b_2;
       let wb_1 = beta_to_why3_r b_1 in 
       let wb_2 = beta_to_why3_r b_2 in 
       T.t_app_infer why3_unionset [wb_1; wb_2]

    | BIntersect (b_1,b_2) -> 
            let wb_1 = beta_to_why3_r b_1 in 
            let wb_2 = beta_to_why3_r b_2 in 
            T.t_app_infer why3_interset [wb_1; wb_2]

    | BSetDiff (b_1,b_2) ->  
        let wb_1 = beta_to_why3_r b_1 in 
        let wb_2 = beta_to_why3_r b_2 in 
        T.t_app_infer why3_diffset [wb_1; wb_2]

let why3_eq_cs (s1, s2) =
    let  w_s1 = why3_i_term s1 in
    let  w_s2 = why3_i_term s2 in
    T.ps_app why3_eq [w_s1; w_s2]

let why3_leq_cs (s1, s2) =
    let  w_s1 = why3_i_term s1 in
    let  w_s2 = why3_i_term s2 in
    T.ps_app why3_leq [w_s1; w_s2]

  let why3_lt_cs (s1, s2) =
    let  w_s1 = why3_i_term s1 in
    let  w_s2 = why3_i_term s2 in
    T.ps_app why3_lt [w_s1; w_s2]

(* let why3_leq_cs_int (s1, s2) =
    let  w_s1 = why3_i_term s1 in
    let  w_s2 = why3_i_term s2 in
    T.ps_app why3_leq_int [w_s1; w_s2] *)








let rec cs_to_why3 cs  =
   match cs with
  | CTrue -> T.t_true
  | CFalse -> T.t_false
  | CEq (s1,s2) -> why3_eq_cs (s1,s2)
  | CLeq (s1,s2) -> why3_leq_cs (s1,s2)
  | CLt (s1,s2) -> why3_lt_cs (s1,s2)
  (* | CLeqI (s1,s2) -> why3_leq_cs_int (s1,s2) *)
  | CImpl (c1, c2) -> (* why_debug "!*! imply cs_to_why3: @[%a, %a@]" Print.pp_cs c1 Print.pp_cs c2; *) 
      T.t_binary T.Timplies (cs_to_why3 c1) (cs_to_why3 c2)
  | CAnd (c1, c2) ->  T.t_binary T.Tand (cs_to_why3 c1) (cs_to_why3 c2)
  | COr (c1, c2) ->  T.t_binary T.Tor (cs_to_why3 c1) (cs_to_why3 c2)
  | CForall (v,i, s, c) ->   
  (*  why_debug "!*! forall cs_to_why3: @[%a@]" Print.pp_cs c;  *)
  let w_v = get_why3_var1 v.v_name  s in T.t_forall_close [w_v] []  ( cs_to_why3 c)
  | CExists (v, i, s, c) ->let w_v = get_why3_var2 v.v_name s in
    T.t_exists_close [w_v] []  ( cs_to_why3 c)
  | CArrPos (o, l) ->  let  arr= why3_i_term o in 
                        let pos= why3_i_term l in  
                         T.t_true
  | CBetaIn (l, b) -> 
            let  arr= beta_to_why3_r b in 
                        let pos= why3_i_term l in  
                        let pos_int = why3_real2int pos in 

                           T.t_app_infer why3_mem [ pos_int; arr]
  | CBetaEq (b_1,b_2 ) -> 
              let  arr1= beta_to_why3_r b_1 in
              let  arr2= beta_to_why3_r b_2 in
              T.t_app_infer why3_seteq [arr1; arr2]
  | CBetaSub (b_1,b_2 ) -> 
              let  arr1= beta_to_why3_r b_1 in
              let  arr2= beta_to_why3_r b_2 in
              T.t_app_infer why3_subset [arr1; arr2]
  | CNot c -> let wc= cs_to_why3 c in 
              T.t_not wc 



let new_theory d t =
  let t' = WT.use_export (WT.create_theory (I.id_fresh "newT")) t in
  WT.close_theory (WT.add_param_decl t' d)


let why3_translate cs =
  H.clear !vmap; H.clear !vmap2;
  try
   (* why_debug "!*! Constraint to translate: @[%a@]" Print.pp_cs cs;  *)

    cs_to_why3 cs
  with Ty.TypeMismatch(ty1, ty2) ->
    why_error  dp "Why3 type mismatch: @[%a@] vs @[%a@]" WP.print_ty ty1 WP.print_ty ty2



(*int *****************)

let rec why3_i_term_int s =
  match s with
  | IVar v         -> 
   why_debug dp "!*! IVar: @[%a@]" Print.pp_vinfo v; 
  begin
    match v.v_type with
    | BiIVar -> 
    begin
    if H.mem !vmap v.v_name 
    then let w_v = T.t_var (H.find !vmap v.v_name) in w_v
    else why_error dp "1 Can't find var %a already declared" Print.pp_vinfo v
    end
    | _ -> begin
      if H.mem !vmap2 v.v_name 
    then let w_v = T.t_var (H.find !vmap2 v.v_name) in w_v
    else why_error dp " 2 Can't find var %a already declared" Print.pp_vinfo v
    end
  end
  | IZero          ->  why3_int 0
  | ISucc s        ->  let w_s = why3_i_term_int s in
    T.t_app_infer why3_add_int [why3_int 1; w_s]
  | IAdd (s1,s2)   ->  
    let w_s1 = why3_i_term_int s1 in
    let w_s2 = why3_i_term_int s2 in
    T.t_app_infer why3_add_int [w_s1; w_s2]
  | IMinus (s1,s2) ->  
    let w_s1 = why3_i_term_int s1 in
    let w_s2 = why3_i_term_int s2 in
    T.t_app_infer why3_sub_int [w_s1; w_s2]
    
   | IMult (s1,s2)  ->  
    let w_s1 = why3_i_term_int s1 in
    let w_s2 = why3_i_term_int s2 in
    T.t_app_infer why3_mult_int [w_s1; w_s2]

    | IBetaABS (b) -> 
      let arr =  beta_to_why3 b in 
        T.t_app_infer why3_abs [arr]
    | IBetaMin (b) -> 
      let arr =  beta_to_why3 b in 
        T.t_app_infer why3_min_elt [arr]
    | IMinimal (i1, i2) -> 
          let w_1 = why3_i_term_int i1 in 
          let w_2 = why3_i_term_int i2 in
          let min =  T.t_app_infer why3_intmin [w_1;w_2] in 
         min
   | IMaximal (i1, i2) -> 
          let w_1 = why3_i_term_int i1 in 
          let w_2 = why3_i_term_int i2 in
          let min =  T.t_app_infer why3_intmax [w_1;w_2] in 
         min

    | IConst c -> if ((int_of_float c) <0) then why3_int 0 else why3_int (int_of_float (ceil c))
    | _ ->  why3_int 0

  and  beta_to_why3 beta  =
    match beta with
    | BIO ->  T.t_app_infer why3_fullset [] 
    | BIE ->  T.t_app_infer why3_emptyset [] 
    | BVar v -> T.t_var (get_why3_var_mix v.v_name Arr)
    | BRange (i_1,i_2) -> 
        let w_1 =  why3_i_term_int i_1 in 
         let w_2 =  why3_i_term_int i_2 in 
            T.t_app_infer why3_rangeset [w_1 ; w_2] 
    | BPos i -> 
        let w =  why3_i_term_int i in 
            T.t_app_infer why3_rangeset [w ; w] 
    | BUnion (b_1, b_2 )  ->
       let wb_1 = beta_to_why3 b_1 in 
       let wb_2 = beta_to_why3 b_2 in 
       T.t_app_infer why3_unionset [wb_1; wb_2]

    | BIntersect (b_1,b_2) -> 
            let wb_1 = beta_to_why3 b_1 in 
            let wb_2 = beta_to_why3 b_2 in 
            T.t_app_infer why3_interset [wb_1; wb_2]

    | BSetDiff (b_1,b_2) ->  
        let wb_1 = beta_to_why3 b_1 in 
        let wb_2 = beta_to_why3 b_2 in 
        T.t_app_infer why3_diffset [wb_1; wb_2]


let why3_leq_cs_int (s1, s2) =
    let  w_s1 = why3_i_term_int s1 in
    let  w_s2 = why3_i_term_int s2 in
    T.ps_app why3_leq_int [w_s1; w_s2]

let why3_lt_cs_int (s1, s2) =
    let  w_s1 = why3_i_term_int s1 in
    let  w_s2 = why3_i_term_int s2 in
    T.ps_app why3_lt_int [w_s1; w_s2]

 let why3_eq_cs_int (s1, s2) =
    let  w_s1 = why3_i_term_int s1 in
    let  w_s2 = why3_i_term_int s2 in
    T.ps_app why3_eq_int [w_s1; w_s2]   




let rec cs_to_why3_int cs  =
   match cs with
  | CTrue -> T.t_true
  | CFalse -> T.t_false
  | CEq (s1,s2) -> why3_eq_cs_int (s1,s2)
  | CLeq (s1,s2) -> why3_leq_cs_int (s1,s2)
  | CLt (s1,s2) -> why3_lt_cs_int (s1,s2)
  (* | CLeqI (s1,s2) -> why3_leq_cs_int (s1,s2) *)
  | CImpl (c1, c2) -> (* why_debug "!*! imply cs_to_why3: @[%a, %a@]" Print.pp_cs c1 Print.pp_cs c2;  *)
      T.t_binary T.Timplies (cs_to_why3_int c1) (cs_to_why3_int c2)
  | CAnd (c1, c2) ->  T.t_binary T.Tand (cs_to_why3_int c1) (cs_to_why3_int c2)
  | COr (c1, c2) ->  T.t_binary T.Tor (cs_to_why3_int c1) (cs_to_why3_int c2)
  | CForall (v,i, s, c) ->   
   why_debug dp "!*! forall cs_to_why3: @[%a @]" Print.pp_vinfo v ;  
  let w_v = ( get_why3_var_mix v.v_name s )  in T.t_forall_close [w_v] []  ( cs_to_why3_int c)
  | CExists (v, i, s, c) ->
   why_debug dp "!*! exists cs_to_why3: @[%a@]" Print.pp_vinfo v;  
  let w_v = (get_why3_var_mix2 v.v_name s )  in
    T.t_exists_close [w_v] []  ( cs_to_why3_int c)
  | CArrPos (o, l) ->  let  arr= why3_i_term_int o in 
                        let pos= why3_i_term_int l in  
                         T.t_true
  | CBetaIn (l, b) -> 
            let  arr= beta_to_why3 b in 
                        let pos= why3_i_term_int l in  
                           T.t_app_infer why3_mem [ pos; arr]
  | CBetaEq (b_1,b_2 ) -> 
              let  arr1= beta_to_why3 b_1 in
              let  arr2= beta_to_why3 b_2 in
              T.t_app_infer why3_seteq [arr1; arr2]
  | CBetaSub (b_1,b_2 ) -> 
              let  arr1= beta_to_why3 b_1 in
              let  arr2= beta_to_why3 b_2 in
              T.t_app_infer why3_subset [arr1; arr2]
  | CNot c -> let wc= cs_to_why3_int c in 
              T.t_not wc 


let why3_translate_int cs =
  H.clear !vmap; H.clear !vmap2;
  try
  (* why_debug "!*! Constraint to translate int: @[%a@]" Print.pp_cs cs;  *)

    cs_to_why3_int cs
  with Ty.TypeMismatch(ty1, ty2) ->
    why_error  dp "Why3 type mismatch: @[%a@] vs @[%a@]" WP.print_ty ty1 WP.print_ty ty2

