open Syntax
open IndexSyntax
open Format


let pp_bop fmt (p : Syntax.bop) = 
  match p with
    | Add           -> fprintf fmt " + "
    | Sub         -> fprintf fmt " - "
    | Mul           -> fprintf fmt " * "
    | Div           -> fprintf fmt " / " 
    | Or            -> fprintf fmt " || "
    | And           -> fprintf fmt " && "
    | Xor           -> fprintf fmt " ^ " 
    | Equal         -> fprintf fmt " = "
    | Leq           -> fprintf fmt " <= " 
    | Geq           -> fprintf fmt " >= " 
    | Less          -> fprintf fmt " < " 
    | Greater       -> fprintf fmt " > "
    | Setminus      -> fprintf fmt " \ "

let pp_uop fmt (p : Syntax.uop) = 
  match p with
    | Sign          -> fprintf fmt "sign"
    | Log            -> fprintf fmt "log"

let rec pp_expression fmt (e : Syntax.expr) = 
  match e with
  | Var v             -> fprintf fmt " Var %s " v.v_name
  | Prim p            -> (match p with 
    | PrimInt i       -> fprintf fmt " Int %d " i
    | PrimUnit        -> fprintf fmt " Unit "
    | PrimReal r      -> fprintf fmt " Real %f " r
    )              
  | True              -> fprintf fmt " True " 
  | False             -> fprintf fmt " False "
  | Pair(e1, e2)      -> fprintf fmt " (%a, %a)"  pp_expression e1 pp_expression e2
  | App (e1, e2)      -> fprintf fmt " @[%a@] @[%a@] " pp_expression e1  pp_expression e2
  | Fix(f, x, e3)     -> fprintf fmt " Fix %s (%a ). @\n@[<hov 1> %a@]@\n" f.v_name pp_expression(x) pp_expression(e3)
  | Fst e             -> fprintf fmt " Fst %a " pp_expression(e)
  | Snd e             -> fprintf fmt " Snd %a " pp_expression(e)
  | If(e, e1, e2)     -> fprintf fmt " If %a Then @\n @[<hov 1> %a@]@\n Else @\n @[<hov 1> %a@]@\n" pp_expression(e)  pp_expression(e1) pp_expression(e2)
  | Mech e            -> fprintf fmt " Mech( %a )" pp_expression(e)
  | Let(x, e1, e2)    -> fprintf fmt " @[<v>@[<hov> Let %s =@;<1 1>@[%a@]@] in@ %a@]" x.v_name pp_expression(e1) pp_expression(e2)
  | Nil               -> fprintf fmt " [] "
  | Cons(e1, e2)      -> fprintf fmt " %a :: %a " pp_expression(e1) pp_expression(e2)
  | Bop(p, e1, e2)    -> fprintf fmt " @[%a@] %a @[%a@] " pp_expression(e1) pp_bop(p) pp_expression(e2)
  | Uop(p, e)         -> fprintf fmt " %a ( %a ) " pp_uop(p)  pp_expression(e)
  | IApp e            -> fprintf fmt " %a []" pp_expression(e)
  | ILam e            -> fprintf fmt " Lam. %a " pp_expression(e)
  | _                 -> fprintf fmt " new "

