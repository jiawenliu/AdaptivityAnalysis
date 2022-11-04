open Syntax

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

let eval e env = 
   let rec eval_aexpr a env = 
    match a with
    | Aint n -> string_of_int n
    | Avar var -> let value = Hashtbl.find_opt env var.v_name in Option.value value ~default:var.v_name
    | Aaop  (aop, a_1 , a_2) -> (eval_aexpr a_1 env) ^ (print_aop aop) ^ (eval_aexpr a_2 env) 
   in 
   match e with
  | Eaexpr a -> eval_aexpr a env
  | Ebexpr _ -> "0"  

let infer program = 
  (*store the result of label -> weight*)
  let weight_table = Hashtbl.create 500 in
  (* store x -> k, y ->2 ....**)
  let env = Hashtbl.create 500 in
  let lcom_list = tranform_2_lcom_list program in
  let handle_while lc b l  = 
    let _ = print_lcommand lc in 
    let _ = print_bexpr b in 
    let label_int = print_label l in
    Hashtbl.add  weight_table label_int "while_executed_times"
     in
  let handle_if lc_1 lc_2 b l = 
    let _ = print_lcommand lc_1 in 
    let _ = print_lcommand lc_2 in 
    let _ = print_bexpr b in 
    let label_int = print_label l in
    Hashtbl.add  weight_table label_int "if_executed_times"
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
  List.iter handle_toplevel_lcom lcom_list ; 
  Hashtbl.fold (fun x v acc -> (x,v)::acc) weight_table []

 let print_weight_list wl = 
  Printf.printf "\nStart of the program infered weight" ;
    List.iter (fun (x,v) -> Printf.printf "(%d,%s)" x v)  wl;
    Printf.printf "End of the program infered weight\n" ;