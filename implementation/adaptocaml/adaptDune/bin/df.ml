open Core
open Cfg

type domain = Syntax.label list

type sigma =  domain String.Map.t

let rec string_of_dom = function
  | [] -> "[]"
  | h :: l -> Format.sprintf "%d :: %s" (Syntax.print_label h) (string_of_dom l) 

let string_of_sigma sigma =
  let show_abstract_val ~key:variable ~data:abstract_value values =
    Format.sprintf "%s%s = %s; " values variable (string_of_dom abstract_value)
  in
  let values = String.Map.fold sigma ~init:"" ~f:show_abstract_val in
  Format.sprintf "[ %s]" values

  type rd_results = sigma Int.Map.t

let string_of_results results =
  let show_result ~key:location ~data:sigma results =
      Format.sprintf "%s\n%d: %s" results location (string_of_sigma sigma)
    in
    Int.Map.fold results ~init:"Results before node n" ~f:show_result

    