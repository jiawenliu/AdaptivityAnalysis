open Core
open Cfg
open Syntax

type domain = Syntax.label list

type sigma =  domain String.Map.t

 let kill program =  function
   | Testblock (_,_) ->  [] 
   | Assignblock (var, _ , _) -> 
     let kill_list =
      let l_list = defs var.v_name program in
      let _ = Printf.printf "in kill, l_list size = %d\n" (List.length l_list) in
      List.map ~f:(fun l -> (var.v_name, print_label l)) l_list in 
    [ (var.v_name, print_label Bot ) ] @ kill_list
   | Queryblock (var, _, _) -> 
    let kill_list =
      let l_list = defs var.v_name program in
      List.map ~f:(fun l -> (var.v_name, print_label l)) l_list in 
    [ (var.v_name, print_label Bot ) ] @ kill_list

let gen  =  function
| Testblock (_,_) ->  [] 
| Assignblock (var, _ , l) ->  [(var.v_name, print_label l)]
| Queryblock (var, _, l) -> [(var.v_name, print_label l)]

(* two associate list a, b, return a - b**)
let list_minus (a) (b) =
  let remove_b pair = not (List.mem b pair
   ~equal:(fun p1 p2 ->
       match p1, p2 with 
       | (s1,i1) ,(s2,i2) when String.equal s1 s2 -> Int.equal i1 i2 
       | _ -> false))
   in    
  List.filter a ~f:remove_b 

(* in and out, input is label, out put is [(x, 5)]*) 
let in_init program =
    let vars = Cfg.assign_vars program in
    List.map ~f:(fun var -> (var, print_label Syntax.Bot)) vars 

 let out l (in_list:(string*int) list ) (cfg_result:Cfg.t) =  
     let label_int = Syntax.print_label l in
     let target_block = Int.Map.find_exn cfg_result.node_map label_int in
     let gen_list = gen target_block in
     let kill_list = kill (cfg_result.program) target_block in
     let minus_list = list_minus in_list kill_list in
     gen_list @ minus_list

(* let in l       *)


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

