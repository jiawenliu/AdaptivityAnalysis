(* ---------------------------------------------------------------------- *)
(* Implementation for Adaptive Analysis                                   *)
(* Abstract Syntax Tree for types and expressions                         *)
(* ---------------------------------------------------------------------- *)

open Format
open Support.FileInfo
open IndexSyntax
open Constr 


(* Execution modes  *)
type mode =
    MaxEx
  | MinEx

(* Types *)

(* Unary primitive types *)
type un_ty_prim =
    UPrimInt
  | UPrimUnit
  | UPrimBool

(* Binary primitive types *)
type bi_ty_prim =
    BPrimInt
  | BPrimUnit
  | BPrimBool

let proj_prim_bi_ty bprim = 
match bprim with
      BPrimInt  -> UPrimInt
    | BPrimUnit -> UPrimUnit
    | BPrimBool -> UPrimBool

let lift_prim_bi_ty uprim = 
match uprim with
      UPrimInt  -> BPrimInt
    | UPrimUnit -> BPrimUnit
    | UPrimBool -> BPrimBool

type predicate = (var_info*iterm) list  


(* Unary types *)
type un_ty =
  (* Primitive types *)
  | UTyPrim  of un_ty_prim

  (* ADT *)
  | UTySum     of un_ty * un_ty
  | UTyProd    of un_ty * un_ty
  (* Functional type *)
  | UTyArr     of un_ty * mode * iterm * un_ty

  (* Quantified types *)
  | UTyForall of var_info * sort * mode * iterm * un_ty
  | UTyExists of var_info * sort * un_ty

  (* List types *)
  | UTyList    of iterm * un_ty

  (* Constrained types *)
  | UTyCs    of constr * un_ty
  | UTyCsImp of constr * un_ty
   (*monadic*)
  | UInt of iterm
  | UMonad of predicate* var_info* un_ty * iterm * mode * predicate
  | UArray of var_info* iterm * un_ty



(* Substitution un_ty[I/i] for index vars *)
let rec un_ty_subst i it uty = 
  let f_it = (iterm_subst i it) in
  let utf = un_ty_subst i it in
  match uty with
  | UTyPrim tp            -> UTyPrim tp
  (* ADT *)
  | UTySum(uty1, uty2)    -> UTySum(utf uty1, utf uty2)
  | UTyProd(uty1, uty2)   -> UTyProd(utf uty1, utf uty2)

  (* Functional type *)
  | UTyArr(uty1, mo, k, uty2) -> UTyArr(utf uty1, mo, f_it k, utf uty2)
                                    
  (* Quantified types *)
  | UTyForall(b, s, mu, k, uty')-> UTyForall(b, s, mu, f_it k,  utf uty')
  | UTyExists(b, s, uty')  -> UTyExists(b, s, utf uty')

  (* Dependent types *)
  | UTyList (sz, uty')     -> UTyList(f_it sz, utf uty')

  (* Constrained types *)
  | UTyCs (cs, uty')      -> UTyCs(constr_subst i it cs, utf uty')
   | UTyCsImp (cs, uty')      -> UTyCsImp(constr_subst i it cs, utf uty')
  (*monadic*)
  | UInt (b) -> UInt (f_it b )
  | UMonad (p,g,uty,k,m,q) -> UMonad(p,g, utf uty, f_it k, m , q )
  | UArray (g, i, uty) -> UArray(g, f_it i, utf uty)


(* Binary Operations    *)
type bop = Plus | Minus | Mul | Div | Or | And | Xor | Equal | Leq | Geq | Less | Greater

(* Unary Operations   *)
type uop = Lg | Sign

(* Expressions   *)
type expr =
  | Var         of string
  | Const_i     of int
  | Const_f     of float
  | True
  | False
  | Pair        of expr * expr
  | App         of expr * expr
  | Fix         of expr * expr * expr(* unsure *) 
  | Fst         of expr
  | Snd         of expr
  | If          of expr * expr * expr
  | Mech        of expr
  | Let         of expr * expr * expr
  | Nil
  | Cons        of expr * expr
  | Bop         of bop * expr * expr
  | Uop         of uop * expr


 type value = 
  | V_True
  | V_False
  | V_Const     of int
  | V_Fix       of expr * ((expr * value) list) (* unsure *) 
  | V_Pair      of value * value
  | V_Nil
  | V_Cons      of value * value
  | V_Error


type trace = 
  | T_Var        of string
  | T_Eval       of trace * trace * (expr * expr) * trace (* unsure *) 
  | T_Fix        of expr
  | T_Pair       of trace * trace
  | T_Fst        of trace
  | T_Snd        of trace
  | T_True
  | T_False
  | T_Iftrue     of trace * trace
  | T_Iffalse    of trace * trace
  | T_Const      of int
  | T_Mech       of trace
  | T_Nil
  | T_Cons       of trace * trace
  | T_Let        of expr * trace * trace
  | T_Error

                         
(* Primitive Terms *)
type exp_prim =
    PrimTUnit
  | PrimTInt    of int
  | PrimTBool   of bool


(*********************************************************************)
(* Expressions with information for error messages *)
type expr =
  (* Variable*)
  | Var of info * var_info
        
  (* Primitive exps *)
  | Prim of info * exp_prim
    
  (* Function definition and application *)
  | Fix of info * var_info * var_info * expr
  | App of info * expr * expr

  (* List constructors and pattern match *)
  | Nil of info                         		
  | Cons of info * expr * expr
  | CaseL of info * expr * expr *
            var_info * var_info * expr
            
  (* ADT: sum types  *)
  | Inl   of info * expr
  | Inr   of info * expr
  | Case  of info * expr * var_info * expr *
             var_info * expr
  
  (* ADT: product types  *)
  | Pair  of info * expr * expr
  | Fst   of info * expr
  | Snd   of info * expr

  (* If-then-else *)
  | IfThen of info * expr * expr * expr

  (* Annotated exprs *)
  | UAnno of info * expr * un_ty * iterm
  | BAnno of info * expr * bi_ty * iterm
  | BAnnoM of  info * expr * bi_ty * bi_ty * iterm* iterm

  (* Let-binding *)
  | Let of info * var_info * expr * expr

  (* Index abstraction and application *)
  | ILam of info * expr
  | IApp of info * expr

  (* Existential index intro and elim forms*)
  | Pack of info * expr
  | Unpack of info * expr * var_info * expr

  (* Constrained expressions *)
  | CExpr of info * expr  (*celim*)
  | CLet of info * var_info * expr * expr
  | Contra of info
  (*monadic expression*)
  | Alloc of info* expr * expr
  | Update of info * expr * expr * expr
  | Read of info * expr * expr
  | Return of info * expr 
  | Letm of info * var_info * expr * expr
  | Split of info * expr * constr
  | FIXEXT of info * un_ty * var_info * var_info * expr
  | SWITCH of info * expr * bi_ty * iterm * constr


let rec is_equal_exp eL eR : bool = 
  match eL, eR with
  | Var(_, v1), Var(_, v2) -> v1 = v2
  | Prim(_, p1), Prim(_, p2) -> p1 = p2
  | Fix(_,f1, x1, e1), Fix(_,f2, x2, e2) -> f1 = f2 && x1 = x2 && is_equal_exp e1 e2
  | FIXEXT (_,uty,f1, x1, e1), FIXEXT (_,uty',f2, x2, e2) -> f1 = f2 && x1 = x2 && is_equal_exp e1 e2
  | Split (_, e1, c1), Split (_, e2,c2) -> is_equal_exp e1 e2
  | Inl(_, e), Inl(_, e') | Inr(_, e), Inr(_, e') 
  | Fst(_, e), Fst(_, e') | Snd(_, e), Snd(_, e')
  | Pack(_, e), Pack (_, e') 
  | ILam(_, e), ILam (_, e') 
  | IApp(_, e), IApp (_, e') 
  | CExpr(_, e), CExpr(_, e') -> is_equal_exp e e'

  | App(_, e1, e2), App(_, e1', e2') 
  | Cons(_, e1, e2), Cons(_, e1', e2') 
  | Pair(_, e1, e2), Pair(_, e1', e2') -> is_equal_exp e1 e1' && is_equal_exp e2 e2'

  | Case(_, e, x, e1, y, e2), Case(_, e', x', e1', y', e2') ->  x = x' && y = y' && is_equal_exp e1 e1' && is_equal_exp e2 e2'

  | CaseL(_, e, e1, h, tl, e2), CaseL(_, e', e1', h', tl', e2') -> h = h' && tl = tl' && is_equal_exp e e' && is_equal_exp e1 e1' && is_equal_exp e2 e2'
  | Let (_, x, e1, e2), Let (_, x', e1', e2')  
  | Unpack(_, e1, x, e2), Unpack(_, e1', x', e2')  
  | CLet (_, x, e1, e2), CLet (_, x', e1', e2') -> x = x' && is_equal_exp e1 e1' && is_equal_exp e2 e2'
  | IfThen (_, e, e1, e2), IfThen (_, e', e1', e2') -> is_equal_exp e e' && is_equal_exp e1 e1' && is_equal_exp e2 e2'
  | Nil i, Nil i' -> true
  | Contra i, Contra i' -> true
  | UAnno(i, e, uty, k), UAnno(i', e', uty', k') -> is_equal_exp e e' && k = k' && uty = uty'
  | BAnno(i, e, bty, k), BAnno(i', e', bty', k') -> is_equal_exp e e' && k = k' && bty = bty'
  | BAnnoM(i, e, b1, b2, k1, k2), BAnnoM(i', e', b1', b2', k1', k2') -> is_equal_exp e e'
  
   (*monadic expressions*)
  | Alloc(_,e1,e2), Alloc(_,e1',e2')  -> is_equal_exp e1 e1' && is_equal_exp e2 e2'
  | Update(_,e1,e2,e3), Update(_,e1',e2',e3') -> is_equal_exp e1 e1' && is_equal_exp e2 e2' && is_equal_exp e3 e3' 
  | Read(_,e1,e2), Read(_,e1',e2') -> is_equal_exp e1 e1' && is_equal_exp e2 e2'
  | Return(_,e), Return(_,e') -> is_equal_exp e e'
  | Letm (_, x, e1, e2), Letm (_, x', e1', e2') -> x = x' && is_equal_exp e1 e1' && is_equal_exp e2 e2'
  | _,_ -> eL = eR


let rec exp_free_vars (e: expr) = 
  match e with
  | Prim (_, _) 
  | Nil _  
  | Contra _   -> []
  | Var(_, x)  -> [x.v_name]

  | Inl(_, e) 
  | Fst(_, e)  
  | Snd(_, e)  
  | Inr(_, e)  
  | UAnno (_, e, _, _) 
  | BAnno (_, e, _, _) 
  | BAnnoM(_, e, _, _, _, _)
  | ILam(_, e)  
  | IApp(_, e) 
  | Pack(_, e)
  | CExpr(_, e)  -> exp_free_vars e
                    
  | App(_, e1, e2)                                                          
  | Pair(_, e1, e2) 
  | Cons(_, e1, e2) -> exp_free_vars e1 @ exp_free_vars e2

  | IfThen (_, e, e1, e2) -> exp_free_vars e1 @ exp_free_vars e2 @ exp_free_vars e
                                                               
  | Fix (_, f, x, e') ->
    List.filter (fun vi_x -> vi_x != f.v_name && vi_x != x.v_name) (exp_free_vars e')
  | FIXEXT (_,uty, f, x, e') ->
    List.filter (fun vi_x -> vi_x != f.v_name && vi_x != x.v_name) (exp_free_vars e')
  | Split (_, e, c) ->  exp_free_vars e
  | CaseL(_, e', e1, h, tl, e2) -> 
    (exp_free_vars e') @  (exp_free_vars e1) @ 
    (List.filter (fun x -> x != h.v_name && x != tl.v_name) (exp_free_vars e2))

  | Case(_, e, x, e1, y, e2) ->(exp_free_vars e) @ 
    (List.filter (fun vi_x -> vi_x != x.v_name ) (exp_free_vars e1)) @ 
    (List.filter (fun vi_x -> vi_x != y.v_name ) (exp_free_vars e2))
  
  | Let (_, x, e1, e2)  
  | Unpack(_, e1, x, e2)  
  | CLet (_, x, e1, e2)   ->  
    (exp_free_vars e1) @ (List.filter (fun vi_x -> vi_x != x.v_name)  (exp_free_vars e2))
 (* manadic*)
  | Return(_,e) -> exp_free_vars e
  | Alloc(_,e1,e2) 
  | Read(_,e1,e2) -> exp_free_vars e1 @ exp_free_vars e2
  | Update(_,e1,e2,e3) ->  exp_free_vars e1 @ exp_free_vars e2 @ exp_free_vars e3
  | Letm(_, x, e1, e2)  -> exp_free_vars e1 @ (List.filter (fun vi_x -> vi_x != x.v_name )  (exp_free_vars e2) )
  | SWITCH (_, _,_,_,_) -> []

(************************************************************************)
(* Info extraction *)
let rec expInfo = function 
    Var(i, _) 
  | Prim(i, _)

  | Fix (i, _, _, _)

  | App(i, _, _)
  | Nil i  
  | Cons(i, _, _)
  | CaseL(i, _, _, _, _, _)

  | Inl(i, _)
  | Inr(i, _)
  | Case(i, _, _, _, _, _)

  | Pair(i, _, _)
  | Fst(i, _)
  | Snd(i, _)

  | IfThen (i, _, _, _)

  | UAnno (i, _, _, _)
  | BAnno (i, _, _, _)
  | BAnnoM(i, _, _, _,_, _)
  | Let (i, _, _, _)

  | ILam(i, _)
  | IApp(i, _) 

  | Pack(i, _)
  | Unpack(i, _, _, _)

  | CExpr(i, _)
  | CLet (i, _, _, _)
  | Contra i           
  | Return(i,_) 
  | Alloc(i,_,_) 
  | Read(i,_,_) 
  | Update(i,_,_,_) 
  | Letm(i,_,_,_)  
  | FIXEXT (i, _, _, _, _) 
  | Split (i, _, _) 
  | SWITCH (i, _,_,_,_)  -> i

(* et check_lists_leq l1 l2 : bool =
  try List.for_all2 (fun i_1 i_2 -> i_1 <= i_2) l1 l2 with Invalid_argument "ararys length fails" -> false

let check_lists_eq l1 l2 : bool =
  try List.for_all2 (fun i_1 i_2 -> i_1 = i_2) l1 l2 with Invalid_argument "ararys length fails" -> false
                    
                    
let check_arrays_leq (arr_1: iterm) (arr_2: iterm) : bool =
      match arr_1 , arr_2 with
        | IArray a_1 , IArray a_2 ->
            let l_1 = Array.to_list a_1  in 
            let l_2 = Array.to_list a_2 in
            check_lists_leq l_1 l_2
        | _ , _ -> false *)

let sort_alg (i_1:int) (i_2:int) :int =
    i_1 - i_2






       

  let minus_cost (k_1: iterm) (k_2 : iterm) : iterm =
    iterm_simpl (IMinus( k_1, k_2) )

  let predicate_remove (p1:predicate) (p2: predicate) : predicate =
    List.filter ( fun (g, iarr) -> not (List.mem_assoc g p2) )   p1

   let predicate_intersect (p1:predicate) (p2: predicate) : predicate =
    List.filter ( fun (g, iarr) ->  (List.mem_assoc g p2) )   p1

       let predicate_union (p1:predicate) (p2: predicate) : predicate =
    List.append p1 p2
