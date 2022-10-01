(* ---------------------------------------------------------------------- *)
(* Implementation for Adaptive Analysis                                   *)
(* Abstract Syntax Tree for types and expressions                         *)
(* ---------------------------------------------------------------------- *)
open Printf

type var_info = {
  v_name  : string;
}

(* arithmetic Operations    *)
type aop = Add | Sub | Mul | Div |  Xor 

(*comparsonn operation**)
type cop = | Equal 
| Leq | Geq | LessThan | GreaterThan  

(* boolean Operations   *)
type bop = Or | And 

type label = 
  | Label of int 
  | Bot

type a_expr = 
  | Aint of int
  | Avar of var_info
  | Aaop of (aop * a_expr * a_expr)

and
b_expr = 
| BTrue
| BFalse
| BNeg of b_expr
| Bbop of (bop * b_expr * b_expr)
| Bcop of (cop * a_expr * a_expr)


and
expr = 
 | Eaexpr of a_expr
 | Ebexpr of b_expr

and
query_expr = 
 | Qalpha
 | Qaexpr of a_expr
 | Qaop of (aop * query_expr * query_expr)
 | Qchi of a_expr


and
lcommand = 
  Skip 
| Assign of var_info * expr * label
| Query of  var_info * query_expr * label
| While of b_expr * lcommand * label
| Seq of lcommand * lcommand 
| If of b_expr * lcommand * lcommand * label

type block =
| Assignblock of var_info * expr * label 
| Queryblock of var_info * query_expr * label
| Testblock of b_expr * label

type lvar = LabelVar of string * int

let print_lvar lvar =
  match lvar with
    | LabelVar (s,i) -> sprintf "(%s)^{%d}" s i

let print_label l = 
  match l with
  | Label n ->  n
  | _ -> 0
  
let print_aop aop =
  match aop  with
  | Add  -> "+"
  | Sub  -> "-"
  | Mul  -> "*"
  | Div  -> "÷"
  | Xor  -> "xor" 

let print_bop bop =
  match bop with
  | Or -> "∨"
  | And -> "&"

let print_cop cop =
  match cop with
  | Equal -> "=="
  | Leq  -> "<="
  | Geq  -> ">="
  | LessThan  -> "<"
  | GreaterThan  -> ">"


let rec  print_aexpr a = 
  match a with
  | Aint n -> string_of_int n
  | Avar var -> var.v_name
  | Aaop  (aop, a_1 , a_2) -> (print_aexpr a_1 ) ^ (print_aop aop) ^ (print_aexpr a_2) 

let rec print_bexpr b =
  match b with
  | BTrue  -> "true"
  | BFalse  -> "false"
  | BNeg  b ->  sprintf "not(%s)" (print_bexpr b)
  | Bbop (bop, b_1 , b_2) ->  (print_bexpr b_1 ) ^ (print_bop bop) ^ (print_bexpr b_2) 
  | Bcop  (cop , a_1 , a_2) -> (print_aexpr a_1 ) ^ (print_cop cop) ^ (print_aexpr a_2) 

let print_expr e =
   match e with
 | Eaexpr a -> print_aexpr a
 | Ebexpr b -> print_bexpr b
  
let rec print_qexpr q =
  match q with
  | Qalpha  -> "α"
  | Qaexpr a -> print_aexpr a 
  | Qaop (aop, q_1 , q_2) -> (print_qexpr q_1 ) ^ (print_aop aop) ^ (print_qexpr q_2) 
  | Qchi a -> sprintf "χ[%s]" (print_aexpr a)

let print_block b =
  match b with
  | Assignblock (var, e, l) -> sprintf " AssignBlock: [ %s = %s ]^{%d}" var.v_name (print_expr e) (print_label l)
  | Queryblock (var, q, l) -> sprintf "QueryBlock: [ %s = query( %s) ]^{%d}" var.v_name (print_qexpr q) (print_label l)
  | Testblock (b, l) ->     sprintf "TestBlock:[%s]^{%d}" (print_bexpr b) (print_label l)

  let isQuery b =
    match b with
    | Queryblock (_, _, _) -> 1
    | _ ->     0
  

let rec print_lcommand lcom = 
  match lcom with
  | Skip -> "Skip"
  | Assign  (var ,e , l) -> sprintf "[ %s = %s ]^{%d}" var.v_name (print_expr e) (print_label l)
  | Query   (var,  q , l)  -> sprintf "[ %s = query( %s) ]^{%d}" var.v_name (print_qexpr q) (print_label l)
  | While ( b, lc, l) -> sprintf "While [ %s ]^{%d} Do {%s}" (print_bexpr b) (print_label l) (print_lcommand lc)  
  | Seq (lc_1,  lc_2 )-> sprintf "%s ; \n %s" (print_lcommand lc_1) (print_lcommand lc_2)
  | If ( b, lc_1 , lc_2,  l) -> sprintf " If [ %s ]^{%d} \n then {%s} else { %s }"  (print_bexpr b) (print_label l) (print_lcommand lc_1)  (print_lcommand lc_2)


  let print_flow flow =
    List.fold_left (fun () (x,  y) -> Printf.printf "edge from %d to %d \n" (print_label x) (print_label y)  ) () flow 

  
    let print_out_flow oc flow =
      List.fold_left (fun () (x,  y) -> Printf.fprintf oc "(%d, %d)," (print_label x) (print_label y)  ) () flow   