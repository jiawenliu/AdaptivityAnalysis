open Syntax
open Printf


let check_nested_while program = 
  let rec contains_while lcom = 
    match lcom with
    |  Skip _  -> false
    | Assign ( _ , _ , _) -> false
    | Query ( _ ,  _ , _ ) -> false
    | While ( _ , _ , _ ) ->  true   
    | Seq ( lc_1,  lc_2 ) -> contains_while lc_1 || contains_while lc_2
    | If ( _ , lc_1 , lc_2 , _ ) -> contains_while lc_1 || contains_while lc_2 
  in 
  let rec inner_check lcom = 
     match lcom with
     |  Skip _  -> false
     | Assign ( _ , _ , _) -> false
     | Query ( _ ,  _ , _ ) -> false
     | While ( _ , lc , _ ) ->  contains_while lc       
     | Seq ( lc_1,  lc_2 ) -> inner_check lc_1 || inner_check lc_2
     | If ( _ , lc_1 , lc_2 , _ ) -> inner_check lc_1 || inner_check lc_2
  in
inner_check program  

let tranform_2_lcom_list program =
  let rec split lcom  = 
    match lcom with
  | Skip l  ->  [(Skip l)]
  | Assign ( var , e , l) -> [ Assign ( var , e , l) ]
  | Query ( var ,  q , l ) -> [ Query ( var ,  q , l ) ]
  | While ( b , lc , l ) -> [ While ( b , lc , l ) ] 
  | Seq ( lc_1,  lc_2 ) -> (split lc_1) @ (split lc_2) 
  | If ( b , lc_1 , lc_2 , l ) -> [If ( b , lc_1 , lc_2 , l )]
in 
  split program
   
let print_lcom_list program =
   let lcom_list = tranform_2_lcom_list program in
   List.iter (fun lcom -> Printf.printf "%s" (print_lcommand lcom))  lcom_list;
   Printf.printf "the program has nested while:%b" (check_nested_while program)

let rec eval_aexpr a env = 
  match a with
  | Aint n -> string_of_int n
  | Avar var -> let value = Hashtbl.find_opt env var.v_name in Option.value value ~default:var.v_name
  | Aaop  (aop, a_1 , a_2) -> (eval_aexpr a_1 env) ^ (print_aop aop) ^ (eval_aexpr a_2 env)

let eval e env = 
   match e with
  | Eaexpr a -> eval_aexpr a env
  | Ebexpr _ -> "0"  

let extract_var_from_aexpr a  = 
   match a with
  | Avar var -> var.v_name
  | _ -> "Only var is expected."

let simplify_gap i_init_value constant = 
  match constant, i_init_value with 
  | "0", _ ->   i_init_value
  | _, "0" -> "-"^constant
  | _ -> i_init_value^"-"^constant

let decide_condition_pattern b env = 
  match b with 
  | Bcop  (cop , a_1 ,  a_2) -> (
      match cop with 
      | Leq 
      | LessThan ->    (* < constant i*) 
        let constant = eval_aexpr a_1 env in 
        let i = extract_var_from_aexpr a_2 in
        let i_init_value = Hashtbl.find_opt env i |> Option.value ~default:i
      in 
       (i, simplify_gap i_init_value constant )
      | Geq
      | GreaterThan 
      | Equal ->   (* > j  0*) 
      let constant = eval_aexpr a_2 env in 
      let i = extract_var_from_aexpr a_1 in
      let i_init_value = Hashtbl.find_opt env i |> Option.value ~default:i
    in 

     (i, simplify_gap i_init_value constant )
  )
  | BTrue -> "true", "inf"
  | BFalse -> "false" , "0"
  | _ -> "not support", "N/A"



let get_variance_of_i_in_aexpr i a tmp_env =
    match a with
  | Aint _ -> "0"
  | Avar _ -> "0"
  | Aaop  (aop, a_1 , a_2) -> (*i = - i , 1*) 
    let i_1 = extract_var_from_aexpr a_1 in 
    let i_2 = extract_var_from_aexpr a_2 in
    match i_1, i_2 with 
    | x , _ when String.equal i x-> let const = eval_aexpr a_2 tmp_env in (match aop with | Sub -> const | Add -> "-"^const |_ -> "0")
    | _, x when String.equal i x -> let const = eval_aexpr a_1 tmp_env in (match aop with | Add -> "-"^const |_ -> "0") 
    | _ -> "0" 

let get_variance_of_i_in_expr i e tmp_env =
   match e with
  | Eaexpr a -> get_variance_of_i_in_aexpr i a tmp_env
  | Ebexpr _ -> "0" 

let variance_of_i_in_loop_body i body env  = 
  let tenv = Hashtbl.copy env in 
  let rec scan lcom acc tmp_env = 
  match lcom with 
  | Skip _  ->  acc, tmp_env
  | Assign ( var , e , _) when String.equal i var.v_name -> (*i related assignment*) 
      let variance = get_variance_of_i_in_expr i e tmp_env in 
          let v = eval e tmp_env in 
          Hashtbl.add tmp_env i v;
          let result = if (String.equal acc "") then variance else acc^"+"^variance in 
          result , tmp_env
 | Assign ( var , e , _) -> 
          let x = var.v_name in  
          let v = eval e tmp_env in 
          Hashtbl.add tmp_env x v;
          acc , tmp_env
  | Query ( _ ,  _ , _ ) -> 
          acc , tmp_env
  | While ( _ , _ , _ ) -> acc, tmp_env
  | Seq ( l_1, l_2 ) -> let acc_1, tmp_env_1 = scan l_1 acc tmp_env in scan l_2 acc_1 tmp_env_1 
  | If ( b , lc_1 , lc_2 , _ ) -> 
      match b with 
      | BTrue -> scan lc_1 acc tmp_env
      | BFalse -> scan lc_2 acc tmp_env
      | _ ->    let tmp_env_1 = Hashtbl.copy tmp_env in 
                let tmp_env_2 = Hashtbl.copy tmp_env in 
        let acc_1, _ = scan lc_1 acc tmp_env_1 in
        let acc_2, _ = scan lc_2 acc tmp_env_2 in 
        let combined_acc =  
        if (String.equal acc_1 acc_2) then acc_1 else (sprintf "(%s || %s)" acc_1 acc_2) in 
        combined_acc , tmp_env     
  in 
  let variance, _ = scan body "" tenv  in 
    variance 


let infer program oc blocks = 
  (*store the result of label -> weight*)
  let weight_table = Hashtbl.create 500 in
  (* store x -> k, y ->2 ....**)
  let env = Hashtbl.create 500 in
  let lcom_list = tranform_2_lcom_list program in
  let rec update_weight_table_in_while_body body inferred_occurence = 
    match body with 
    | Skip l  ->  let label_int = print_label l in Hashtbl.add  weight_table label_int inferred_occurence 
    | Assign ( var , e , l) -> 
            let x = var.v_name in  
            let v = eval e env in 
            let label_int = print_label l in
            Hashtbl.add env x v;
            Hashtbl.add  weight_table label_int inferred_occurence
    | Query ( var ,  _ , l ) -> 
            let x = var.v_name in  
            let label_int = print_label l in
            Hashtbl.add env x "*";
            Hashtbl.add weight_table label_int inferred_occurence 
    | While ( b , lc , l ) ->  (*we do not support nested while now*)
           let i, initial_gap = decide_condition_pattern b env in 
           let variance = variance_of_i_in_loop_body i lc env in 
           let inferred_occurence_of_while = 
            if (String.equal variance "1") then initial_gap else
            sprintf "(%s)/(%s)" initial_gap variance in 
            let combined_occurence = sprintf "(%s)*(%s)" inferred_occurence inferred_occurence_of_while in
            let label_int = print_label l in
           Hashtbl.add weight_table label_int combined_occurence; 
          update_weight_table_in_while_body lc combined_occurence 

    | Seq ( l_1,  l_2 ) -> update_weight_table_in_while_body l_1 inferred_occurence ; 
                         update_weight_table_in_while_body l_2 inferred_occurence  
    | If ( b , lc_1 , lc_2 , l ) -> 
           (match b with 
           | BTrue -> update_weight_table_in_while_body lc_1 inferred_occurence ; update_weight_table_in_while_body lc_2 "0"
           | BFalse -> update_weight_table_in_while_body lc_2 inferred_occurence ; update_weight_table_in_while_body lc_1 "0"
           | _ ->  update_weight_table_in_while_body lc_1 inferred_occurence ; 
                   update_weight_table_in_while_body lc_2 inferred_occurence) ;
          let label_int = print_label l in
          Hashtbl.add weight_table label_int inferred_occurence 
  in
  let handle_while lc b l  = 
    let i, initial_gap = decide_condition_pattern b env in 
    let variance = variance_of_i_in_loop_body i lc env in 
    Printf.printf "\ninfer weight handle while, i:%s, initia_gap:%s, variance:%s" i initial_gap variance ;
    let inferred_occurence_of_while = 
      if (String.equal variance "1") then initial_gap else
      sprintf "(%s)/(%s)" initial_gap variance in 
    let _ = print_lcommand lc in 
    let _ = print_bexpr b in 
    let label_int = print_label l in
    Hashtbl.add weight_table label_int inferred_occurence_of_while; 
    update_weight_table_in_while_body lc inferred_occurence_of_while
     in
     let rec update_weight_table_in_if_body body inferred_occurence = 
      match body with 
      | Skip l  ->  let label_int = print_label l in Hashtbl.add  weight_table label_int inferred_occurence 
      | Assign ( var , e , l) -> 
              let x = var.v_name in  
              let v = eval e env in 
              let label_int = print_label l in
              Hashtbl.add env x v;
              Hashtbl.add  weight_table label_int inferred_occurence
      | Query ( var ,  _ , l ) -> 
              let x = var.v_name in  
              let label_int = print_label l in
              Hashtbl.add env x "*";
              Hashtbl.add weight_table label_int inferred_occurence 
      | While ( b , lc , l ) -> handle_while lc b l
      | Seq ( l_1,  l_2 ) -> update_weight_table_in_if_body l_1 inferred_occurence ; 
               update_weight_table_in_if_body l_2 inferred_occurence  
      | If ( b , lc_1 , lc_2 , l ) -> 
             (match b with 
             | BTrue -> update_weight_table_in_if_body lc_1 inferred_occurence ; update_weight_table_in_if_body lc_2 "0"
             | BFalse -> update_weight_table_in_if_body lc_2 inferred_occurence ; update_weight_table_in_if_body lc_1 "0"
             | _ ->  update_weight_table_in_if_body lc_1 inferred_occurence ; 
             update_weight_table_in_if_body lc_2 inferred_occurence) ;
            let label_int = print_label l in
            Hashtbl.add weight_table label_int inferred_occurence 
    in
  let handle_if lc_1 lc_2 b l = 
    let inferred_occurence = "1" in 
    (match b with 
    | BTrue -> update_weight_table_in_if_body lc_1 inferred_occurence ; update_weight_table_in_if_body lc_2 "0"
    | BFalse -> update_weight_table_in_if_body lc_2 inferred_occurence ; update_weight_table_in_if_body lc_1 "0"
    | _ ->  update_weight_table_in_if_body lc_1 inferred_occurence ; 
    update_weight_table_in_if_body lc_2 inferred_occurence) ;
   let label_int = print_label l in
   Hashtbl.add weight_table label_int "1" 
    in
  let handle_toplevel_lcom lcom = 
    match lcom with
    | Skip l  ->  let label_int = print_label l in Hashtbl.add  weight_table label_int "1" 
    | Assign ( var , e , l) -> 
            let x = var.v_name in  
            let v = eval e env in 
            let label_int = print_label l in
            Hashtbl.add env x v;
            Hashtbl.add  weight_table label_int "1" 
    | Query ( var ,  _ , l ) -> 
            let x = var.v_name in  
            let label_int = print_label l in
            Hashtbl.add env x "*";
            Hashtbl.add weight_table label_int "1" 
    | While ( b , lc , l ) -> handle_while lc b l
    | Seq ( _,  _ ) -> ()
    | If ( b , lc_1 , lc_2 , l ) -> handle_if lc_1 lc_2 b l
  in
  List.iter handle_toplevel_lcom lcom_list; 
  let get_weight block = 
    let x = get_label_from_block block in
    Hashtbl.find_opt weight_table x |> Option.value ~default:"Unknown weight" 
  in 
  List.fold_left ( fun () block -> Printf.fprintf oc "%s," (get_weight block ) ) () blocks;            
  Hashtbl.fold (fun x v acc -> (x,v)::acc) weight_table []

 let print_weight_list wl = 
  Printf.printf "Loose Weight Inference: \n" ;
    List.iter (fun (x,v) -> Printf.printf "(%d,%s)" x v)  wl;
           (* Printf.printf "End of the program infered weight\n" ; *)