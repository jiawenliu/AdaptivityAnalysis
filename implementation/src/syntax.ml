(* ---------------------------------------------------------------------- *)
(* Implementation for Adaptive Analysis                                   *)
(* Abstract Syntax Tree for types and expressions                         *)
(* ---------------------------------------------------------------------- *)

open Format
open IndexSyntax

(* Types *)

(* Unary primitive types *)
type ty_prim =
    Ty_PrimInt
  | Ty_PrimUnit
  | Ty_PrimReal

(* Primitive Terms *)
type exp_prim =
    PrimUnit
  | PrimInt    of int
  | PrimReal   of float


(* Binary Operations    *)
type bop = Add | Sub | Mul | Div | Or | And | Xor | Equal 
| Leq | Geq | Less | Greater | Setminus | Dot 

(* Unary Operations   *)
type uop = Log | Sign



type dmap = (var_info * dterm) list

and 

(* Types *)
ty =
  (* Primitive types *)
  | Ty_Prim       of ty_prim

  | Ty_Bool

  (* Pair *)
  | Ty_Prod       of ty * ty
  (* Functional type *)
  | Ty_Arrow      of ty * dterm * dmap * iterm * ty

  (* Quantified types *)
  | Ty_Forall     of var_info * sort * dmap * iterm * ty
  | Ty_Exists     of var_info * sort * ty
  | Ty_IntIndex   of iterm

  (* Boxed Types *)
  | Ty_Box        of ty

  (* List types *)
  | Ty_List       of ty

and
                    

(* Expressions   *)
expr =
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

  | Let         of var_info * iterm * expr * expr

(*  | Case        of expr * var_info * expr * var_info * expr
*)
  (* Functional Expressions *)
  | Fix         of var_info * var_info * ty * expr
  | App         of expr * expr
  | Mech        of expr

  (* List *)
  | Nil
  | Cons        of expr * expr

  (* Parameterized Constant*)
  | Bernoulli   of expr
  | Uniform     of expr * expr

  (* Parameterized Constant*)
  | Pack        of expr
  | Unpack      of expr * var_info * expr


  (* Binary and Unary Arithmetic Operations*)
  | Bop         of bop * expr * expr
  | Uop         of uop * expr

  (* Indexed Expressions *)
  | ILam        of expr
  | IApp        of expr


(* VALUES   *)
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
  | Tr_Var        of var_info
  | Tr_App        of trace * trace * (var_info * var_info) * trace 
  | Tr_Fix        of expr (* Unsure *)
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
  | Tr_Bernoulli  of expr
  | Tr_Uniform
  | Tr_Error




(* Substitution ty[I/i] for index vars *)
(* More Carefule *)
let rec ty_subst i it ty = 
  let f_it = (iterm_subst i it) in
  let utf = ty_subst i it in
  match ty with
  | Ty_Prim tp            -> Ty_Prim tp

  | Ty_Bool               -> Ty_Bool
  (* ADT *)
  | Ty_Prod(ty1, ty2)   -> Ty_Prod(utf ty1, utf ty2)

  (* Functional type *)
  | Ty_Arrow(ty1, i, d, k, ty2) -> Ty_Arrow(utf ty1, i, d, f_it k, utf ty2)

  (* Quantified types *)
  | Ty_Forall(b, s, d, z, ty')  -> Ty_Forall(b, s, d, z,  utf ty')
  | Ty_Exists(b, s, ty')  -> Ty_Exists(b, s, utf ty')
  | Ty_IntIndex(k)       -> Ty_IntIndex(f_it k)

  (* Boxed Type *)
  | Ty_Box(ty)                -> Ty_Box( utf ty )
  (* List types *)
  | Ty_List (ty')        -> Ty_List( utf ty')



let rec is_equal_exp eL eR : bool = 
  match eL, eR with
  | Var(v1), Var( v2) -> v1 = v2
  | Prim(p1), Prim( p2) -> p1 = p2

  | Fix(f1, x1, ty1, e1), Fix(f2, x2, ty2, e2) 
    -> f1 = f2 && ty1 = ty2 && x1 = x2 && is_equal_exp e1 e2
  
  | Pack( e), Pack (e')
  | Mech( e), Mech (e')
  | Fst( e), Fst( e') | Snd( e), Snd( e') -> is_equal_exp e e'
  | If ( e, e1, e2), If ( e', e1', e2') -> is_equal_exp e e' && is_equal_exp e1 e1' && is_equal_exp e2 e2'

  | Nil , Nil  -> true

  | Unpack (e1, x, e2), Unpack(e1', x', e2') -> is_equal_exp e1 e1' && is_equal_exp e2 e2' && x = x'

  | App( e1, e2), App( e1', e2')
  | Cons( e1, e2), Cons( e1', e2') 
  | Pair( e1, e2), Pair( e1', e2') -> is_equal_exp e1 e1' && is_equal_exp e2 e2'


  | Let ( x, i, e1, e2), Let ( x', i', e1', e2')  
    ->  x = x' && i = i'  && is_equal_exp e1 e1' && is_equal_exp e2 e2'
(*  | Case( e, x, e1, y, e2), Case( e', x', e1', y', e2') 
    -> x = x' && y = y' && is_equal_exp e1 e1' && is_equal_exp e2 e2'
*)
  | Bernoulli v, Bernoulli v'  -> is_equal_exp v  v'
  | Uniform(v1, v2), Uniform(v1', v2')  -> is_equal_exp v1 v1' && is_equal_exp v2 v2'

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
                                                               
  | Fix ( f, x, t, e') ->
    List.filter (fun vi_x -> vi_x != f.v_name) (exp_free_vars e')

(*  | Case( e, x, e1, y, e2) ->(exp_free_vars e) @ 
    (List.filter (fun vi_x -> vi_x != x.v_name ) (exp_free_vars e1)) @ 
    (List.filter (fun vi_x -> vi_x != y.v_name ) (exp_free_vars e2))
*)  
  | Let ( x, i, e1, e2) -> (exp_free_vars e1 ) @
    (List.filter (fun vi_x -> vi_x != x.v_name ) (exp_free_vars e2))

  | ILam( e)
  | IApp( e) -> (exp_free_vars e)

  | _ -> []








(************************************************************************)
(* Info extraction *)
let type_of_prim t = match t with
    PrimUnit       -> Ty_Prim Ty_PrimUnit
  | PrimInt i      -> Ty_IntIndex(IConst(i)) (* UTyPrim UPrimInt *)
  | PrimReal _     -> Ty_Prim Ty_PrimReal
