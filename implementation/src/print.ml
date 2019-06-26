open Syntax
open IndexSyntax

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

let u_sym x = Symbols.string_of_symbol x


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
  fprintf fmt "%s \n\n" "let dataset = [ [1.0;1.0;1.0;1.0] ; [1.0;1.0;1.0;1.0] ; [1.0;1.0;1.0;1.0] ; [1.0;1.0;1.0;1.0] ] "


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
  | App (e1, e2)      -> fprintf fmt " @[%a@] @[%a@] " pp_expression e1  pp_expression e2
  | Fix(f, x, e3)     -> 
    if(f.v_name = "_")
    then  
      fprintf fmt "(fun (%a ) -> @\n@[<hov 1> %a@]@\n)" pp_expression(x) pp_expression(e3)
    else
      fprintf fmt " let rec %s (%a ) = @\n@[<hov 1> %a@]@\n" f.v_name pp_expression(x) pp_expression(e3)
  | Fst e             -> fprintf fmt " fst %a " pp_expression(e)
  | Snd e             -> fprintf fmt " snd %a " pp_expression(e)
  | If(e, e1, e2)     -> fprintf fmt " if(%a) then @\n @[<hov 1> %a@]@\n else @\n @[<hov 1> %a@]@\n" pp_expression(e)  pp_expression(e1) pp_expression(e2)
  | Mech e            -> fprintf fmt " mech(%a) " pp_expression(e)
  | Let(x, e1, e2)    -> 
  (
    match e1 with
    | Fix(f, _, e3)   -> 
      if(f.v_name = "_")
      then fprintf fmt " @[<v>@[<hov> let %s =@;<1 1>@[%a@]@] in@ %a@]" x.v_name pp_expression(e1) pp_expression(e2)
      else 
        fprintf fmt "%a \nlet %s = %s\n%a" pp_expression(e1) x.v_name f.v_name pp_expression(e2)
    | _ -> fprintf fmt " @[<v>@[<hov> let %s =@;<1 1>@[%a@]@] in@ %a@]" x.v_name pp_expression(e1) pp_expression(e2)
  )
  | Nil               -> fprintf fmt " [] "
  | Cons(e1, e2)      -> fprintf fmt " %a :: %a " pp_expression(e1) pp_expression(e2)
  | Bop(p, e1, e2)    -> 
  (
    match p with
    | Contains        -> fprintf fmt "(List.exists (fun a -> if (a = %a) then true else false) %a)" pp_expression(e2) pp_expression(e1)
    | Setminus        -> fprintf fmt "(listminus %a %a)" pp_expression(e1) pp_expression(e2)
    | _               -> fprintf fmt " ((%a)%a(%a)) " pp_expression(e1) pp_bop(p) pp_expression(e2)
  )

  | Uop(p, e)         -> fprintf fmt " %a (%a) " pp_uop(p)  pp_expression(e)
  | IApp e            -> fprintf fmt " %a " pp_expression(e)
  | ILam e            -> fprintf fmt " %a " pp_expression(e)
  | Bernoulli(v)      -> fprintf fmt "(sample_bernoulli(%a))" pp_expression(v)
  | Uniform(v1, v2)   -> fprintf fmt "(sample_uniform %a %a)" pp_expression(v1) pp_expression(v2)
  | _                 -> fprintf fmt " new "



let rec pp_primutype fmt ty = match ty with
    Ty_PrimInt     -> fprintf fmt "@<1>%s" (u_sym Symbols.Int)
  | Ty_PrimUnit    -> fprintf fmt "@<1>%s" (u_sym Symbols.Unit)
  | Ty_PrimBool    -> fprintf fmt "@<1>%s" (u_sym Symbols.Bool)
  | Ty_PrimReal    -> fprintf fmt "@<1>%s" (u_sym Symbols.Real)

let rec pp_list pp fmt l = match l with
    []         -> fprintf fmt ""
  | hd :: []  -> fprintf fmt "%a" pp hd
  | hd :: tl -> fprintf fmt "%a,@ %a" pp hd (pp_list pp) tl


let rec pp_sort fmt s = match s with
    Adapt       -> fprintf fmt "%s" "adapt"



let rec pp_iterm fmt ty = match ty with
  | IConst i            -> fprintf fmt " Index %d " i
  | IVar v              -> fprintf fmt " Index %s " v.v_name
  | IAdd(i1, i2)        -> fprintf fmt " %a + %a " pp_iterm i1 pp_iterm i2
  | ISub(i1, i2)        -> fprintf fmt " %a - %a " pp_iterm i1 pp_iterm i2
  | IMaximal(i1, i2)    -> fprintf fmt " Max(%a, %a) " pp_iterm i1 pp_iterm i2

let rec pp_type fmt ty = match ty with
  | Ty_Prim tp               -> fprintf fmt "%a " pp_primutype tp

  | Ty_Prod(ty1, ty2)        -> fprintf fmt "(%a @<1>%s @[<h>%a@]) " pp_type ty1 (u_sym Symbols.Times) pp_type ty2
  | Ty_Arrow(ity, q, d, a, oty) 
                             -> fprintf fmt "%a , %d @<1>%s %a  @[<h>%a@] " pp_type ity q (u_sym Symbols.Arrow) pp_iterm a pp_type oty

  | Ty_Forall(i, s, ty1)     -> fprintf fmt "@<1>%s %s :: %a. %a " (u_sym Symbols.Forall) i.v_name pp_sort s pp_type ty1
  | Ty_Exists(i, s, ty1)     -> fprintf fmt "@<1>%s %s :: %a. %a " (u_sym Symbols.Exists) i.v_name pp_sort s pp_type ty1
  | Ty_Index(i, ty1)         -> fprintf fmt "%a[%a] " pp_type ty1 pp_iterm i

  | Ty_Box ty1               -> fprintf fmt "@<1>%s %a " (u_sym Symbols.Box) pp_type ty1

  | Ty_List ty1              -> fprintf fmt "%a list " pp_type ty1


