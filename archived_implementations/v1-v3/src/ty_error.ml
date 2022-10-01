(* ---------------------------------------------------------------------- *)
(* Errors                                                                 *)
(* ---------------------------------------------------------------------- *)

open Syntax
open Format
open Print

(* Errors *)
type ty_error_elem =
| TypeMismatch of ty * ty
| NotSubtype   of ty * ty
| WrongShape   of ty * string
| SwitchPMatch
| Internal of string



let pp_tyerr ppf s = match s with
 | TypeMismatch(uty1, uty2)-> fprintf ppf "Cannot unify %a with %a" pp_type uty1 pp_type uty2
 | WrongShape(uty, sh)     -> fprintf ppf "Type %a has wrong shape, expected %s type." pp_type uty sh
 | NotSubtype(uty1,uty2)   -> fprintf ppf "Type %a is not a subtype of %a" pp_type uty1 pp_type uty2
 | SwitchPMatch             -> fprintf ppf "Switch pattern match to unary mode"
 | Internal s               -> fprintf ppf "Internal error: %s" s
