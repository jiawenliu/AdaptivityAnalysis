open Syntax
open Format

type cons_info =
    Symb of string 
  | Const of int

type constriant = 
  Reset of var_info * ( var_info option )  * cons_info
| Dec of var_info *  ( var_info  option )  * cons_info
| Inc of var_info *  ( var_info  option )  * cons_info
| Top


type abs_transition = label * label * constriant

let abs_expr var e = 
  match e with 
  | Eaexpr a ->
(    match a with
    | Aint c -> Reset (var, None, Const c)
    | Avar v -> Reset (var, Some v, Const 0)
    | Aaop (Sub, Avar var', Aint c) -> 
      if var = var' then 
        Dec (var, None, Const c)
    else
      Reset (var, Some var', Const c)
    | Aaop (Add, Avar var', Aint c) -> 
      if var = var' then 
        Inc (var, None, Const c)
    else
      Reset (var, Some var', Const c)
      | _ -> Reset (var, None, Symb "INF"))
  | Ebexpr _ -> Reset (var, None, Const 1)

  let rec abs_final (lcom : lcommand) : (label * constriant) list 
  =
    match lcom with
    |  Skip  -> []
    | Assign (var, e, l ) -> [(l, (abs_expr var e))]
    | Query  ( var ,_ , l ) -> [(l, Reset (var, None, Symb "Q" ))]
    | While ( _ , _ , l ) -> [(l, Top)]
    | Seq ( _ ,  lc_2 ) -> abs_final lc_2 
    | If ( _ , lc_1 , lc_2 , _ ) -> (abs_final lc_1) @ (abs_final lc_2)
 
 
   (* Control flow graph *)
 
let rec abs_flow (lcom : lcommand) : abs_transition list =
  match lcom with
  |  Skip  -> []
  | Assign ( _ , _ , _) -> []
  | Query ( _ ,  _ , _ ) -> []
  | While ( _ , lc , l ) ->   (abs_flow lc) @ [(l, Cfg.init lc, Top)] @ 
    (List.map (fun abs_l -> let (l_1, l_constriant) = abs_l in (l_1, l, l_constriant)) (abs_final lc) ) 
  | Seq ( lc_1,  lc_2 ) -> (abs_flow lc_1) @ (abs_flow lc_2) @ 
  (List.map (fun abs_l -> let (l_1, l_constriant) = abs_l in (l_1, Cfg.init lc_2, l_constriant))  (abs_final lc_1) )

  | If ( _ , lc_1 , lc_2 , l ) -> [ ( l, Cfg.init lc_1, Top ) ; (l, Cfg.init lc_2, Top) ] @ (abs_flow lc_1) @ (abs_flow lc_2) 
  


  let infile = ref (None : string option)
(* let outfile = ref (None : string option) *)

let print_const const = 
  match const with
  | Symb s -> s
  | Const i ->  sprintf " %d " i

let print_constriant c = 
  match c with
  | Reset (var, Some var', cons) -> sprintf " Variable Reset: [ %s <= %s + %s ] " var.v_name var'.v_name (print_const cons)
  | Dec (var, Some var', cons) -> sprintf " Variable Decrease: [ %s <= %s - %s ] " var.v_name var'.v_name (print_const cons)
  | Inc (var, Some var', cons) -> sprintf " Variable Increase: [ %s <= %s + %s ] " var.v_name var'.v_name (print_const cons)
  | Reset (var, None, cons) -> sprintf " Variable Reset: [ %s <= %s ] " var.v_name (print_const cons)
  | Dec (var, None, cons) -> sprintf " Variable Decrease: [ %s <= %s - %s ] " var.v_name var.v_name (print_const cons)
  | Inc (var, None, cons) -> sprintf " Variable Increase: [ %s <= %s + %s ] " var.v_name var.v_name (print_const cons)
  | Top -> sprintf "[True]"

let print_abs_flow aflow =
  List.fold_left (fun () (x,  y, c) -> Printf.printf "edge from %d to %d  with constriant: %s \n" (print_label x) (print_label y) 
  (print_constriant c) ) () aflow 


let argDefs = [
    "-i", Arg.String (fun s -> infile := Some s  ), "specify the input file name" ] 
    (* "-o", Arg.String (fun s -> outfile := Some s ), "specify the output file name" *)

let parseArgs () =  
        Arg.parse argDefs 
        (fun s -> 
                match !infile  with 
                      | Some (_) -> printf "%s" "specify "  
                      | None  -> infile := Some (s) ) " " ;
             match !infile  with
                   | Some inf -> inf
                   | None -> printf "specify your input file name by -i infilename"; ""
                   
let parse_prog file =
    let ic = open_in file 
    in
      let lb = (Lexing.from_channel ic) in 
        Parser.toplevel Lexer.main lb

      let _ =
        (* let lexbuf = Lexing.from_channel stdin in *)
        let infile  = parseArgs () in 
          let result = parse_prog infile in
          let final_label = Cfg.final result in 
          let _ =  Printf.printf "final label size: %d" (List.length final_label) in 
            List.fold_left ( fun () label -> Printf.printf "final label : %d" (Syntax.print_label label) ) () final_label;
          let blocks = Cfg.blocks result in
          List.fold_left ( fun () block -> Printf.printf "%s ; \n" (Syntax.print_block block) ) () blocks;
          let string_result = print_lcommand result in
          Printf.printf "The input program is : %s" string_result;
          let aflow = abs_flow result in
          print_abs_flow aflow;
          print_newline();

      