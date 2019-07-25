open Syntax
open IndexSyntax
open Constr
open DMap

open Format

module Symbols = struct
  type pp_symbols =
      Inf
    | Forall
    | Exists
    | Arrow
    | DblArrow
    | Times
    | Int
    | IntR
    | Size
    | Real
    | Bool
    | BoolR
    | Unit
    | UnitR
    | Mu
    | Lambda
    | BigLambda
    | Vdash
    | Leq
    | Top
    | Bot
    | And
    | Or
    | Impl
    | Box

  (* TODO: add summations etc. *)

  let pp_symbol_table s = match s with
      Inf      -> ("inf",     "∞")
    | Forall   -> ("forall ", "∀")
    | Exists   -> ("exits " , "∃")
    | Arrow    -> ("->",      "→")
    | DblArrow -> ("=>",      "⇒")
    | Times    -> ("x",       "⊗")
    | Int      -> ("int",     "ℤ")
    | IntR     -> ("intR",     "ℤᵣ")
    | Size     -> ("nat",     "ℕ")
    | Real     -> ("num",     "ℝ")
    | Bool     -> ("bool",    "𝔹")
    | BoolR    -> ("boolR",    "𝔹ᵣ")
    | Unit     -> ("unit",    "unit")
    | UnitR    -> ("unitR",    "unitᵣ")
    | Mu       -> ("mu",      "μ")
    | Lambda   -> ("\\",      "λ")
    | BigLambda-> ("\\!",  "Λ") 
    | Vdash    -> ("|-",      "⊢")
    | Leq      -> ("<=",      "≤")
    | Top      -> ("true",    "T")
    | Bot      -> ("false",   "⊥")
    | And      -> ("and",      "∧")
    | Or       -> ("or",      "∨")
    | Impl     -> ("-->",     "→")
    | Box      -> ("box",     "□")

  let string_of_symbol s =
    let select = snd in
    select (pp_symbol_table s)
end


let sym x = Symbols.string_of_symbol x


let pp_bop fmt (p : Syntax.bop) = 
  match p with
    | Add           -> fprintf fmt " +. "
    | Sub           -> fprintf fmt " -. "
    | Mul           -> fprintf fmt " *. "
    | Div           -> fprintf fmt " /. " 
    | Or            -> fprintf fmt " || "
    | And           -> fprintf fmt " && "
    | Xor           -> fprintf fmt " ^ " 
    | Equal         -> fprintf fmt " = "
    | Leq           -> fprintf fmt " <= " 
    | Geq           -> fprintf fmt " >= " 
    | Less          -> fprintf fmt " < " 
    | Greater       -> fprintf fmt " > "
    | Setminus      -> fprintf fmt " \ "
    | Dot           -> fprintf fmt " *. "


let pp_uop fmt (p : Syntax.uop) = 
  match p with
    | Sign          -> fprintf fmt "sign"
    | Log           -> fprintf fmt "log"

let rec pp_head fmt =
  fprintf fmt "open Printf \n";
  fprintf fmt "open HeadFile \n\n";
  fprintf fmt "open Distribution \n\n"



(*let rec pp_mechs fmt (mech_name) = 
  match mech_name with
    | Guassian
*)
let rec pp_dataset fmt = 
  fprintf fmt "%s \n\n" "let dataset = [1.0;1.0;1.0;1.0]  "


let rec pp_expression fmt (e : Syntax.expr) = 
  match e with
  | Var v             -> 
  (
    match v.v_name with
      | "foldl" -> fprintf fmt " List.fold_left "
      | _       -> fprintf fmt " %s " v.v_name
  )  
  | Prim p            -> 
    (match p with 
      | PrimInt i       -> fprintf fmt " %f " (float_of_int i)
      | PrimUnit        -> fprintf fmt " () "
      | PrimReal r      -> fprintf fmt " %f " r
    )              
  | True              -> fprintf fmt " true " 
  | False             -> fprintf fmt " false "
  | Pair(e1, e2)      -> fprintf fmt " (%a, %a)"  pp_expression e1 pp_expression e2
  | App (e1, e2)      -> fprintf fmt " (@[%a@] @[%a@]) " pp_expression e1  pp_expression e2
  | Fix(f, x, t, e3)     -> 
    if(f.v_name = "_")
    then  
      fprintf fmt "(fun (%s ) -> @\n@[<hov 1> %a@]@\n)" x.v_name pp_expression(e3)
    else
      fprintf fmt " let rec %s (%s ) = @\n@[<hov 1> %a@]@\n" f.v_name x.v_name pp_expression(e3)
  | Fst e             -> fprintf fmt " fst %a " pp_expression(e)
  | Snd e             -> fprintf fmt " snd %a " pp_expression(e)
  | If(e, e1, e2)     -> fprintf fmt " if(%a) then @\n @[<hov 1> %a@]@\n else @\n @[<hov 1> %a@]@\n" pp_expression(e)  pp_expression(e1) pp_expression(e2)
  | Mech e            -> fprintf fmt " mech %a db " pp_expression(e)
  | Let(x, i, e1, e2)    -> 
  (
    match e1 with
    | Fix(f, _, _, e3)   -> 
      if(f.v_name = "_")
      then fprintf fmt " @[<v>@[<hov> let %s =@;<1 1>@[%a@]@] in@ %a@]" x.v_name pp_expression(e1) pp_expression(e2)
      else 
        fprintf fmt "%a \nlet %s = %s\n%a" pp_expression(e1) x.v_name f.v_name pp_expression(e2)
    | Annotated(e, _, _, _) ->
      (
        match e with
          | Var {v_name = "contra"} -> pp_expression fmt e2 
          | _                       -> fprintf fmt " @[<v>@[<hov> let %s =@;<1 1>@[%a@]@] in@ %a@]" x.v_name pp_expression(e1) pp_expression(e2)
      )
    | _ -> fprintf fmt " @[<v>@[<hov> let %s =@;<1 1>@[%a@]@] in@ %a@]" x.v_name pp_expression(e1) pp_expression(e2)
  )
  | Nil               -> fprintf fmt " [] "
  | Cons(e1, e2)      -> fprintf fmt " %a :: %a " pp_expression(e1) pp_expression(e2)
  | Bop(p, e1, e2)    -> 
  (
    match p with
    | Setminus        -> fprintf fmt "(db_minus %a %a)" pp_expression(e1) pp_expression(e2)
    | _               -> fprintf fmt " ((%a)%a(%a)) " pp_expression(e1) pp_bop(p) pp_expression(e2)
  )

  | Uop(p, e)         -> fprintf fmt " %a (%a) " pp_uop(p)  pp_expression(e)
  | IApp(i, e)            -> fprintf fmt " %a " pp_expression(e)
  | ILam(i, e)            -> fprintf fmt " %a " pp_expression(e)
  | Bernoulli(v)      -> fprintf fmt "(sample_bernoulli(%a))" pp_expression(v)
  | Uniform(v1, v2)   -> fprintf fmt "(sample_uniform %a %a)" pp_expression(v1) pp_expression(v2)
  | Annotated(e, t, d, z)   -> pp_expression fmt e
  | _                 -> fprintf fmt " new "


let rec pp_primutype fmt ty = match ty with
    Ty_PrimInt     -> fprintf fmt "@<1>%s" (sym Symbols.Int)
  | Ty_PrimUnit    -> fprintf fmt "@<1>%s" (sym Symbols.Unit)
  | Ty_PrimReal    -> fprintf fmt "@<1>%s" (sym Symbols.Real)

let rec pp_list pp fmt l = match l with
    []         -> fprintf fmt ""
  | hd :: []  -> fprintf fmt "%a" pp hd
  | hd :: tl -> fprintf fmt "%a,@ %a" pp hd (pp_list pp) tl


let rec pp_sort fmt s = match s with
    Adapt       -> fprintf fmt "%s" "adapt"


(**********************************************************************)
(* Pretty printing for Index term *)

let rec pp_iterm fmt ty = match ty with
  | IConst i            -> fprintf fmt " Index %d " i
  | IVar v              -> fprintf fmt " Index %s " v.v_name
  | IAdd(i1, i2)        -> fprintf fmt " %a + %a " pp_iterm i1 pp_iterm i2
  | ISub(i1, i2)        -> fprintf fmt " %a - %a " pp_iterm i1 pp_iterm i2
  | IMaximal(i1, i2)    -> fprintf fmt " Max(%a, %a) " pp_iterm i1 pp_iterm i2

(**********************************************************************)
(* Pretty printing for Depth term *)

let rec pp_dterm fmt ty = match ty with
  | DConst i            -> fprintf fmt " Depth %d " i
  | DVar v              -> fprintf fmt " Depth %s " v.v_name
  | DBot                -> fprintf fmt " @<1>%s " (sym Symbols.Bot)
  | DInfty              -> fprintf fmt " @<1>%s " (sym Symbols.Inf)
  | DMaximal(d1, d2)    -> fprintf fmt " Max(%a, %a) " pp_dterm d1 pp_dterm d2
  | DAdd(d1, d2)        -> fprintf fmt " %a + %a " pp_dterm d1 pp_dterm d2
  | DSub(d1,d2)         -> fprintf fmt " %a - %a " pp_dterm d1 pp_dterm d2

let rec pp_dlist fmt d =
  match d with
    | []       -> ()
    | (v, depth) :: tl  -> fprintf fmt "(%s: %a), %a" v.v_name pp_dterm depth pp_dlist tl

let rec pp_dmap fmt dmap =
  let dps = DMap.to_dlist dmap in
    let rec pp_dlist fmt d =
      match d with
        | []       -> ()
        | (vname, depth) :: tl  -> fprintf fmt "(%s: %a), %a" vname pp_dterm depth pp_dlist tl
      in
    pp_dlist fmt dps

let pp_adapt fmt z = 
    fprintf fmt "Adapt:(%a)" pp_iterm z



let rec pp_type fmt ty = match ty with
  | Ty_Prim tp               -> fprintf fmt "%a " pp_primutype tp
  | Ty_Bool                  -> fprintf fmt "@<1>%s" (sym Symbols.Bool)

  | Ty_Prod(ty1, ty2)        -> fprintf fmt "(%a @<1>%s @[<h>%a@]) " pp_type ty1 (sym Symbols.Times) pp_type ty2
  | Ty_Arrow(ity, q, d, a, oty) 
                             -> fprintf fmt "%a , %a @<1>%s [%a] %a  @[<h>%a@] " pp_type ity pp_dterm q (sym Symbols.Arrow) pp_dlist d pp_iterm a pp_type oty

  | Ty_Forall(i, s, d, z, ty1)     
      -> fprintf fmt "@<1>%s %s ::(%a; %a) %a. %a " (sym Symbols.Forall) 
      i.v_name pp_dlist d pp_iterm z pp_sort s pp_type ty1
  | Ty_Exists(i, s, ty1)     -> fprintf fmt "@<1>%s %s :: %a. %a " (sym Symbols.Exists) i.v_name pp_sort s pp_type ty1
  | Ty_IntIndex(i)         -> fprintf fmt "Int[%a] " pp_iterm i

  | Ty_Box ty1               -> fprintf fmt "@<1>%s %a " (sym Symbols.Box) pp_type ty1

  | Ty_List ty1              -> fprintf fmt "%a list " pp_type ty1

(**********************************************************************)
(* Pretty printing for constraints *)

let pp_ivar_ctx_elem ppf (v, s) =
    fprintf ppf "%-10s" v.v_name

(*let pp_ivar_ctx = pp_list pp_ivar_ctx_elem
*)


(**********************************************************************)
(* Pretty printing for constraints *)
let rec pp_cs ppf cs =
  match cs with
    | CTrue                -> fprintf ppf "%s" (sym Symbols.Top)
    | CFalse               -> fprintf ppf "%s" (sym Symbols.Bot)
    | CEq(i1, i2)          -> fprintf ppf "%a = %a" pp_iterm i1 pp_iterm i2
    | CLeq(i1, i2)         -> fprintf ppf "%a %s %a" pp_iterm i1 (sym Symbols.Leq) pp_iterm i2
    | CLt(i1, i2)         -> fprintf ppf "%a %s.. %a" pp_iterm i1 ("<") pp_iterm i2 
    | CAnd(cs1, cs2)       -> fprintf ppf "%a %s %a" pp_cs cs1 (sym Symbols.And) pp_cs cs2
    | COr(cs1, cs2)        -> fprintf ppf "(%a) %s (%a)" pp_cs cs1 (sym Symbols.Or) pp_cs cs2
    | CImpl(cs1, cs2)      -> fprintf ppf "%a %s (%a)" pp_cs cs1 (sym Symbols.Impl) pp_cs cs2
    | CForall(bi_x, s, cs) -> fprintf ppf "@<1>%s%s :: %a.@;(@[%a@])" (sym Symbols.Forall) bi_x.v_name pp_sort s pp_cs cs
    | CExists(bi_x, s, cs) -> fprintf ppf "@<1>%s%s :: %a.@;(@[%a@])" (sym Symbols.Exists) bi_x.v_name pp_sort s pp_cs cs
    | CArrPos(o, l)        -> fprintf ppf "%a[%a] =  true" pp_iterm o pp_iterm l       
    | CNot c               ->  fprintf ppf "NOT %a " pp_cs c  
    | CDEq(d1, d2)         -> fprintf ppf "%a = %a" pp_dterm d1 pp_dterm d2
    | CDLeq(d1,d2)         -> fprintf ppf "%a %s %a" pp_dterm d1 (sym Symbols.Leq) pp_dterm d2
    | _ -> ()


let rec main_print s = 
  fprintf std_formatter s




let rec pp_progm fmt expr = 
      pp_head fmt; 
      pp_dataset fmt;
      pp_expression fmt expr
