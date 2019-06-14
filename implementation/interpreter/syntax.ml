(* ---------------------------------------------------------------------- *)
(* Implementation for Adaptive Analysis                                   *)
(* Abstract Syntax Tree for types and expressions                         *)
(* ---------------------------------------------------------------------- *)

open Format
open IndexSyntax


(* Execution modes  *)
type mode =
    Check
  | Infer

(* Types *)

(* Unary primitive types *)
type ty_prim =
    Ty_PrimInt
  | Ty_PrimUnit
  | Ty_PrimBool
  | Ty_PrimReal



(* Types *)
type ty =
  (* Primitive types *)
  | Ty_Prim     of ty_prim

  (* Pair *)
  | Ty_Prod     of ty * ty
  (* Functional type *)
  | Ty_Arr      of ty * mode * iterm * ty

  (* Quantified types *)
  | Ty_Forall   of var_info * sort * iterm * ty
  | Ty_Exists   of var_info * sort * iterm * ty
  | Ty_Index    of sort * iterm * ty

  (* Boxed Types *)
  | Ty_Box      of ty

  (* List types *)
  | Ty_List     of iterm * ty



(* Substitution ty[I/i] for index vars *)
let rec ty_subst i it ty = 
  let f_it = (iterm_subst i it) in
  let utf = ty_subst i it in
  match ty with
  | Ty_Prim tp            -> Ty_Prim tp
  (* ADT *)
  | Ty_Prod(ty1, ty2)   -> Ty_Prod(utf ty1, utf ty2)

  (* Functional type *)
  | Ty_Arr(ty1, mo, k, ty2) -> Ty_Arr(utf ty1, mo, f_it k, utf ty2)

  (* Quantified types *)
  | Ty_Forall(b, s, k, ty')  -> Ty_Forall(b, s, f_it k,  utf ty')
  | Ty_Exists(b, s, k, ty')  -> Ty_Exists(b, s, f_it k, utf ty')
  | Ty_Index(s, k, ty')       -> Ty_Index(s, f_it k, utf ty')

  (* Boxed Type *)
  | Ty_Box(ty)                -> Ty_Box( utf ty )
  (* List types *)
  | Ty_List (i, ty')        -> Ty_List(f_it i,  utf ty')



(* Primitive Terms *)
type exp_prim =
    PrimUnit
  | PrimInt    of int
  | PrimReal   of float


(* Binary Operations    *)
type bop = Add | Sub | Mul | Div | Or | And | Xor | Equal | Leq | Geq | Less | Greater

(* Unary Operations   *)
type uop = Log | Sign

                    

(* Expressions   *)
type expr =
  (* Const *)
  | Prim        of exp_prim
  | True
  | False

  (* Variables and Regular Expressions *)
  | Var         of var_info
  | Pair        of expr * expr
  | Fst         of expr
  | Snd         of expr
  | If          of expr * expr * expr

  | Let         of var_info * expr * expr
  | Case        of expr * var_info * expr * var_info * expr

  (* Functional Expressions *)
  | Fix         of var_info * expr * expr(* unsure *) 
  | App         of expr * expr
  | Mech        of expr

  (* List *)
  | Nil
  | Cons        of expr * expr

  (* Parameterized Constant*)
  | Bernoulli   of exp_prim
  | Uniform     of exp_prim * exp_prim

  (* Parameterized Constant*)
  | Pack        of expr
  | Unpack      of expr * var_info * expr


  (* Binary and Unary Arithmetic Operations*)
  | Bop         of bop * expr * expr
  | Uop         of uop * expr

  (* Indexed Expressions *)
  | ILam        of expr
  | IApp        of expr


let rec is_equal_exp eL eR : bool = 
  match eL, eR with
  | Var(v1), Var( v2) -> v1 = v2
  | Prim(p1), Prim( p2) -> p1 = p2

  | Fix(f1, x1, e1), Fix(f2, x2, e2) -> f1 = f2 && is_equal_exp x1 x2 && is_equal_exp e1 e2
  
  | Pack( e), Pack (e')
  | Mech( e), Mech (e')
  | Fst( e), Fst( e') | Snd( e), Snd( e') -> is_equal_exp e e'
  | If ( e, e1, e2), If ( e', e1', e2') -> is_equal_exp e e' && is_equal_exp e1 e1' && is_equal_exp e2 e2'

  | Nil , Nil  -> true

  | Unpack (e1, x, e2), Unpack(e1', x', e2') -> is_equal_exp e1 e1' && is_equal_exp e2 e2' && x = x'

  | App( e1, e2), App( e1', e2')
  | Cons( e1, e2), Cons( e1', e2') 
  | Pair( e1, e2), Pair( e1', e2') -> is_equal_exp e1 e1' && is_equal_exp e2 e2'


  | Let ( x, e1, e2), Let ( x', e1', e2')  ->  x = x' && is_equal_exp e1 e1' && is_equal_exp e2 e2'
  | Case( e, x, e1, y, e2), Case( e', x', e1', y', e2') 
    -> x = x' && y = y' && is_equal_exp e1 e1' && is_equal_exp e2 e2'

  | Bernoulli v, Bernoulli v'  -> v = v'
  | Uniform(v1, v2), Uniform(v1', v2')  -> v1 = v1' && v2 = v2'

  | ILam( e), ILam(e')
  | IApp( e), IApp( e') -> is_equal_exp e e'

  | _   -> false


let rec exp_free_vars (e: expr) = 
  match e with
  | Prim (_)
  | Bernoulli _
  | Uniform _
  | Nil   -> []
  | Var( x)  -> [x.v_name]

  | Fst( e)  
  | Snd( e)  
    -> exp_free_vars e
                    
  | App( e1, e2)                                                          
  | Pair( e1, e2) 
  | Cons( e1, e2) -> exp_free_vars e1 @ exp_free_vars e2

  | If ( e, e1, e2) -> exp_free_vars e1 @ exp_free_vars e2 @ exp_free_vars e
                                                               
  | Fix ( f, x, e') ->
    List.filter (fun vi_x -> vi_x != f.v_name) (exp_free_vars e')

  | Case( e, x, e1, y, e2) ->(exp_free_vars e) @ 
    (List.filter (fun vi_x -> vi_x != x.v_name ) (exp_free_vars e1)) @ 
    (List.filter (fun vi_x -> vi_x != y.v_name ) (exp_free_vars e2))
  
  | Let ( x, e1, e2) -> (exp_free_vars e1 ) @
    (List.filter (fun vi_x -> vi_x != x.v_name ) (exp_free_vars e2))

  | ILam( e)
  | IApp( e) -> (exp_free_vars e)

  | _ -> []


 type value = 
  | V_True
  | V_False
  | V_Prim      of exp_prim
  | V_Fix       of expr * ((expr * value) list) (* unsure *) 
  | V_Pair      of value * value
  | V_Nil
  | V_Cons      of value * value
  | V_Error


type trace = 
  | Tr_Var        of string
  | Tr_Eval       of trace * trace * (expr * expr) * trace (* unsure *) 
  | Tr_Fix        of expr
  | Tr_Pair       of trace * trace
  | Tr_Fst        of trace
  | Tr_Snd        of trace
  | Tr_True
  | Tr_False
  | Tr_Iftrue     of trace * trace
  | Tr_Iffalse    of trace * trace
  | Tr_Const      of int
  | Tr_Mech       of trace
  | Tr_Nil
  | Tr_Cons       of trace * trace
  | Tr_Let        of expr * trace * trace
  | Tr_Error



(************************************************************************)
(* Info extraction *)
(*let rec expInfo = function 
    Var(i, _) 
  | Prim(i, _)

  | Fix (i,   _)

  | App(i,  _)
  | Nil i  
  | Cons(i,  _)
  | CaseL(i,     _)

  | Inl(i, _)
  | Inr(i, _)
  | Case(i,     _)

  | Pair(i,  _)
  | Fst(i, _)
  | Snd(i, _)

  | IfThen (i,   _)

  | UAnno (i,   _)
  | BAnno (i,   _)
  | BAnnoM(i,    _)
  | Let (i,   _)

  | ILam(i, _)
  | IApp(i, _) 

  | Pack(i, _)
  | Unpack(i,   _)

  | CExpr(i, _)
  | CLet (i,   _)
  | Contra i           
  | Return(i,_) 
  | Alloc(i,_) 
  | Read(i,_) 
  | Update(i,_) 
  | Letm(i,_)  
  | FIXEXT (i,    _) 
  | Split (i,  _) 
  | SWITCH (i, _)  -> i
*)

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
