open Core
open Cfg
open Syntax

type domain = string * int

type sigma =  domain list

type rd_results = sigma Int.Map.t 

 let kill program =  function
   | Testblock (_,_) ->  [] 
   | Assignblock (var, _ , _) -> 
     let kill_list =
      let l_list = defs var.v_name program in
      (* let _ = Printf.printf "in kill, l_list size = %d\n" (List.length l_list) in *)
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

let compare_domain (a:domain) (b:domain) : int =
  let string_cmpare = String.compare (fst a) (fst b) in
 if ( string_cmpare = 0) then Int.compare (snd a) (snd b)
 else string_cmpare


(*
   For Kildall's algorithm ur termination condition relies on the ability to tell whether one
   abstract state is "not equal to" another. Since our abstract state in this analysis is a map
   over variables, you will need to lift the inequality check from individual variables to maps *)
   let sigma_ne (state1 : sigma) (state2 : sigma) : bool = 
      let sorted_1 = List.sort state1 ~compare:compare_domain in
      let sorted_2 = List.sort state2 ~compare:compare_domain in
      let com_result =
      List.compare compare_domain sorted_1 sorted_2 in
      not (com_result = 0)
 

(* two associate list a, b, return a - b**)
let sigma_minus (a:sigma) (b:sigma) =
  let remove_b pair = not (List.mem b pair
   ~equal:(fun p1 p2 ->
       match p1, p2 with 
       | (s1,i1) ,(s2,i2) when String.equal s1 s2 -> Int.equal i1 i2 
       | _ -> false))
   in    
  List.filter a ~f:remove_b 

let sigma_join (a:sigma) (b:sigma) : sigma =
  let combined_result = List.append a b in
  List.dedup_and_sort combined_result ~compare:compare_domain

(* in and out, input is label, out put is [(x, 5)]*) 
let in_init program : sigma =
    let vars = Cfg.assign_vars program in
    List.map ~f:(fun var -> (var, print_label Syntax.Bot)) vars 

 let out (label_int:int) (in_list: sigma  ) (cfg_result:Cfg.t) : sigma=  
     let target_block = Int.Map.find_exn cfg_result.node_map label_int in
     let gen_list = gen target_block in
     let kill_list = kill (cfg_result.program) target_block in
     (* let in_list = Int.Map.find_exn label_int rd in  *)
     let minus_list = sigma_minus in_list kill_list in
     sigma_join gen_list minus_list

 let print_sigma (sigma:sigma) =
  List.fold_left ~f:( fun () (x, v) -> 
    Printf.printf "%s : %d," x v ) ~init:() sigma

let in_f (label_int:int) (rd: rd_results  ) (cfg_result:Cfg.t) :sigma = 
    let pre_map = cfg_result.pre_map in
    let precessors = Int.Map.find_exn  pre_map  label_int in
    List.fold_left  precessors ~init:[]
    ~f:(fun sigma l -> let sigma_l = Int.Map.find_exn rd (print_label l)  in 
        sigma_join sigma sigma_l )

let initial_rd_input (cfg : Cfg.t) : rd_results =
  let inputMap = Int.Map.map cfg.node_map ~f:(fun _ -> []) in
inputMap
 
let kildall (cfg : Cfg.t) : rd_results * rd_results =
  let rec work (inputs_out : rd_results) (inputs_in : rd_results)  = function
        | [] -> (inputs_out, inputs_in) (* while worklist is not empty *)
        | n :: ns -> (* take node n off of the worklist *)
           (* let block = Int.Map.find_exn cfg.node_map n in *)
           let old = Int.Map.find_exn inputs_out n in
           let _ = Printf.printf "old %d\n" n in
           let () = print_sigma old in
           let _ = Printf.printf "end old %d\n" n in
           let in_l = in_f n inputs_out cfg in
           let _ = Printf.printf "in_l %d\n" n in
           let _ = print_sigma in_l in
           let _ = Printf.printf "in_l end %d\n" n in
           let inputs_in' = Int.Map.set inputs_in ~key:n ~data:in_l in
           let out_l = out n in_l cfg in
           let _ = Printf.printf "out_l %d\n" n in
           let _ = print_sigma out_l in
           let _ = Printf.printf "end out_l %d\n" n in
           let inputs_out' = Int.Map.set inputs_out ~key:n ~data:out_l in 
           let worklist' = 
             if (sigma_ne old out_l) then 
           ( let successors = Int.Map.find_exn cfg.suc_map n in
            let successors_int = List.map ~f:(fun l -> print_label l) successors in ns @ successors_int)
               else ns in
              work inputs_out' inputs_in' worklist'                   
      in
      (* This initializes the inputMap, mapping every variable to Top for the first instruction
         in the code/node in the graph, and mapping every variable to Bot for the rest of the nodes. *)
      (* let botSigma = initializeSigma (cfg) (Bot) in
      let topSigma = initializeSigma (cfg) (Top) in *)
      (* let inputMap = Int.Map.map cfg.nodes ~f:(fun k -> botSigma) in
      let inputMap = Int.Map.set inputMap ~key:1 ~data: topSigma in *)
      let inputMap = initial_rd_input cfg in
      let outputMap = initial_rd_input cfg in
     
      (* we use a simple list for the worklist; it's less efficient than various
       * alternatives, but relatively easy to reason about.  You're welcome to use
       * some other datatype if you want, but you're also welcome to just use a list.
       * Your call *)
      work (outputMap) (inputMap) (List.sort (Int.Map.keys cfg.node_map) ~compare:Int.compare)
    
        

