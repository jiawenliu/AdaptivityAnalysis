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

                         
(* Binary types*)
type bi_ty =
  (* Primitive types *)
  | BTyPrim  of bi_ty_prim

  (* ADT *)
  | BTySum     of bi_ty * bi_ty
  | BTyProd    of bi_ty * bi_ty

  (* Functional type *)
  | BTyArr     of bi_ty * iterm * bi_ty

  (* Quantified types *)
  | BTyForall of var_info * sort * iterm * bi_ty
  | BTyExists of var_info * sort * bi_ty

  (********************************************************************)
  (* Dependent types *)
  | BTyList    of iterm * iterm * bi_ty

  (********************************************************************)
  (* Unrelated types *)
  | BTyUnrel    of un_ty * un_ty

  (********************************************************************)
  (* Boxed types *)
  | BTyBox    of bi_ty

 (* Constrained types *)
  | BTyCs    of constr * bi_ty
  | BTyCsImp of constr * bi_ty

  | BMonad of predicate * var_info* bi_ty*iterm * predicate
  | BInt of iterm
  | BArray of var_info*iterm*bi_ty

  let predicate_subst i it p = 
      List.map ( fun (g, iarr) ->
            (g, iterm_subst i it iarr)
          ) p

(* Substitution un_ty[I/i] for index vars *)
let rec bi_ty_subst i it bty = 
  let f_it = (iterm_subst i it) in
  let btf = bi_ty_subst i it in
  let ptf = predicate_subst i it in 
  match bty with
  | BTyPrim tp            -> BTyPrim tp
  (* ADT *)
  | BTySum(bty1, bty2)    -> BTySum(btf bty1, btf bty2)
  | BTyProd(bty1, bty2)   -> BTyProd(btf bty1, btf bty2)

  (* Functional type *)
  | BTyArr(bty1, k, bty2) -> BTyArr(btf bty1, f_it k, btf bty2)
                                    
  (* Quantified types *)
  | BTyForall(b, s, k, bty')-> BTyForall(b, s, f_it k,  btf bty')
  | BTyExists(b, s, bty')  -> BTyExists(b, s, btf bty')

  (* Dependent types *)
  | BTyList (sz, ch, bty') -> BTyList(f_it sz, f_it ch, btf bty')

  (* Unrelated types *)
  | BTyUnrel (uty1, uty2)  -> BTyUnrel (un_ty_subst i it uty1, un_ty_subst i it uty2)

  (* Boxed types *)
  | BTyBox bty'            -> BTyBox (btf bty')

  (* Constrained types *)
  | BTyCs (cs, bty')       -> BTyCs(constr_subst i it cs, btf bty')
  | BTyCsImp (cs, bty')       -> BTyCsImp(constr_subst i it cs, btf bty')
  (*monadic binary types*)
  | BMonad (p,g,bty,k,q) -> BMonad ( ptf p, g, btf bty, f_it k, ptf q)
  | BInt (i) -> BInt (f_it i)
  | BArray(g, i, bty) ->  BArray(g, f_it i, btf bty) 

  
      

 (*helper function*) 
 let rec bi_proj (p:predicate) : predicate=
    match p with
       [] -> []
       | (v,l) :: tl -> 
          match l with
            IArray (a,ls) -> 
                         (v, IArray (a,[]) ) :: (bi_proj tl) 
            |_ -> bi_proj tl

      

(* Projection for binary types *)
let rec bi_ty_proj (isLeft: bool) (bty : bi_ty) : un_ty =
  let btp = bi_ty_proj isLeft in
  match bty with
  | BTyPrim tp            -> UTyPrim (proj_prim_bi_ty tp)
  (* ADT *)
  | BTySum(bty1, bty2)    -> UTySum (btp bty1, btp bty2)
  | BTyProd(bty1, bty2)   -> UTyProd(btp bty1, btp bty2)

  (* Functional type *)
  | BTyArr(bty1, k, bty2) -> if isLeft then UTyArr(btp bty1, MaxEx, IInfty, btp bty2) 
    				       else UTyArr(btp bty1, MinEx, IZero, btp bty2)
        
  (* Quantified types *)
  | BTyForall(b, s, k, bty')-> if isLeft then UTyForall(b, s, MaxEx, IInfty, btp bty') 
    				         else UTyForall(b, s, MinEx, IZero, btp bty')
  | BTyExists(b, s, bty')  -> UTyExists(b, s, btp bty')

  (* Dependent types *)
  | BTyList (sz, ch, bty') -> UTyList(sz, btp bty')

  (* Unrelated types *)
  | BTyUnrel (uty1, uty2)  -> if isLeft then uty1 else uty2

  (* Boxed types *)
  | BTyBox bty'            -> btp bty'

  (* Constrained types *)
  | BTyCs (cs, bty')       -> UTyCs(cs, btp bty')
  | BTyCsImp (cs, bty')       -> UTyCsImp(cs, btp bty')

  | BMonad (p, g, bty, k, q) -> if isLeft 
      then UMonad ( (bi_proj p) , g, btp bty, IInfty, MaxEx, (bi_proj q) )
      else UMonad ( (bi_proj p) , g, btp bty, IZero, MinEx, (bi_proj q) )
  | BInt (i) -> UInt(i)
  | BArray (g, i, bty) -> UArray (g, i, (btp bty ) )
(*********************************************************************)
(* Terms *)


 let rec int_to_speano n = if n = 0 then IZero else ISucc (int_to_speano (n-1))
(* Primitive Terms *)
type exp_prim =
    PrimTUnit
  | PrimTInt    of int
  | PrimTBool   of bool

let un_type_of_prim t = match t with
    PrimTUnit       -> UTyPrim UPrimUnit
  | PrimTInt i      -> UInt (int_to_speano i) (* UTyPrim UPrimInt *)
  | PrimTBool _     -> UTyPrim UPrimBool

let bi_type_of_prim t = match t with
    PrimTUnit       -> BTyPrim BPrimUnit
  | PrimTInt i      -> BInt (int_to_speano i ) (* BTyPrim BPrimInt *)
  | PrimTBool _     -> BTyPrim BPrimBool

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
