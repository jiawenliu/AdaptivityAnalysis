open Syntax
open Core

type edge = label * label
type node = block

type t = {
  program : lcommand;
  nodes : block list;
  node_map: block Int.Map.t;
  edges: (label*label) list;
  pre_map : (label list) Int.Map.t;
  suc_map :(label list) Int.Map.t;
}

let rec init lcom = 
  match lcom with
  |  Skip  -> Bot
  | Assign ( _ , _ , l) -> l
  | Query ( _ ,  _ , l ) -> l
  | While ( _ , _ , l ) -> l
  | Seq ( lc_1,  _ ) -> init lc_1 
  | If ( _ , _ , _ , l ) -> l
  

 let rec final lcom =
   match lcom with
   |  Skip  -> []
   | Assign ( _ , _ , l) -> [l]
   | Query ( _ ,  _ , l ) -> [l]
   | While ( _ , _ , l ) -> [l]
   | Seq ( _ ,  lc_2 ) -> final lc_2 
   | If ( _ , lc_1 , lc_2 , _ ) -> (final lc_1) @ (final lc_2)

   let rec blocks lcom =
     match lcom with
  |  Skip  -> []
  | Assign ( var , e , l) -> [ Assignblock (var, e, l ) ]
  | Query ( var ,  q , l ) -> [ Queryblock (var, q ,l ) ]
  | While ( b , lc , l ) -> [ Testblock (b,l) ] @ (blocks lc)
  | Seq ( lc_1,  lc_2 ) -> (blocks lc_1) @ (blocks lc_2) 
  | If ( b , lc_1 , lc_2 , l ) -> [Testblock (b, l)] @ (blocks lc_1) @ (blocks lc_2)


(*get the int label from a block**)
  let getLabelFromBlock = function
   | Assignblock (_ , _, l ) -> print_label l
   | Queryblock (_, _,l ) -> print_label l
   | Testblock (_ , l) -> print_label l

 (*transalte a list of blocks to a map from int(label) to block**)  
  let blocks2map blocks: block Int.Map.t =
    let add_node blockmap node =
      let keylabel = getLabelFromBlock node in
      Int.Map.set blockmap ~key:keylabel ~data:node in
    List.fold_left blocks ~init:Int.Map.empty ~f:add_node

  (*create a map from int(label) -> a list of its precessors  **)
  let precessor block edges =
    List.fold_left edges  ~init:[] 
    ~f:(fun acc_list edge ->
        match edge with
        | ( pre , b ) when (print_label b) = (getLabelFromBlock block) -> pre::acc_list
        | _ -> acc_list
      )

  let precessor_map nodes edges : (label list) Int.Map.t =
    let add_node pre_map node =
      let keylabel = getLabelFromBlock node in
      let value = precessor node edges in
      Int.Map.set pre_map ~key:keylabel ~data:value in
    List.fold_left nodes ~init:Int.Map.empty ~f:add_node

    (*create a map from int(label) -> a list of its precessors  **)
  let successor block edges =
    List.fold_left edges  ~init:[] 
    ~f:(fun acc_list edge ->
        match edge with
        | (  b, suc ) when (print_label b) = (getLabelFromBlock block) -> suc::acc_list
        | _ -> acc_list
      )

  let successor_map nodes edges : (label list) Int.Map.t =
    let add_node suc_map node =
      let keylabel = getLabelFromBlock node in
      let value = successor node edges in
      Int.Map.set suc_map ~key:keylabel ~data:value in
    List.fold_left nodes ~init:Int.Map.empty ~f:add_node

  (* Control flow graph*)

  let rec flow lcom = 
    match lcom with
    |  Skip  -> []
    | Assign ( _ , _ , _) -> []
    | Query ( _ ,  _ , _ ) -> []
    | While ( _ , lc , l ) ->   (flow lc) @ [(l, init lc)] @ (List.map ~f:(fun l_1 -> (l_1,l)) (final lc) ) 
    | Seq ( lc_1,  lc_2 ) -> (flow lc_1) @ (flow lc_2) @ (List.map ~f:(fun l_1 -> (l_1, init lc_2 )) (final lc_1) )
    | If ( _ , lc_1 , lc_2 , l ) -> [ ( l, init lc_1 ) ; (l, init lc_2) ] @ (flow lc_1) @ (flow lc_2) 

   (*** For  control data flow graph  ***)

   let rec vars_a a : var_info list = 
    match a with
    | Aint _ -> []
    | Avar var -> [var]
    | Aaop  (_, a_1 , a_2) -> (vars_a a_1 ) @ (vars_a a_2) 

  let rec vars_b b : var_info list =
    match b with
  | BTrue  -> []
  | BFalse  -> []
  | BNeg  b ->  vars_b b
  | Bbop (_, b_1 , b_2) ->  (vars_b b_1) @ (vars_b b_2)
  | Bcop  (_ , a_1 , a_2) -> (vars_a a_1 ) @ (vars_a a_2) 

   (* vars return the variables from expression e **)
   let vars e : var_info list=
     match e with
     | Eaexpr a -> vars_a a
     | Ebexpr b -> vars_b b

  let rec qvars q : var_info list = 
    match q with
  | Qalpha  -> []
  | Qaexpr a -> vars_a a 
  | Qaop (_, q_1 , q_2) -> (qvars q_1) @ (qvars q_2)
  | Qchi a -> vars_a a

   (*  label of x in program p, input x, output : a list of labels of x in program p**)  

   let rec defs x = function
   | Skip  -> []
   | Assign ( var , _ , l) -> if (String.equal var.v_name x) then [l] else []
   | Query ( var ,  _ , l ) -> if (String.equal var.v_name x) then [l] else []
   | While ( _ , lc , _ ) ->   defs x lc
   | Seq ( lc_1,  lc_2 ) -> (defs x lc_1) @ (defs x lc_2) 
   | If ( _ , lc_1 , lc_2 , _ ) -> (defs x lc_1) @ (defs x lc_2) 

   
   (*  assigned variables of program, return a list of assigned variables, if we have duplicate x, then just one x**)  
  let assign_vars program = 
   let rec assigned = function
   | Skip  -> []
   | Assign ( var , _ , _) -> [var.v_name]
   | Query ( var ,  _ , _ ) -> [var.v_name]
   | While ( _ , lc , _ ) ->   assigned lc
   | Seq ( lc_1,  lc_2 ) -> (assigned lc_1) @ (assigned lc_2) 
   | If ( _ , lc_1 , lc_2 , _ ) -> (assigned lc_1) @ (assigned lc_2) 
  in 
     let result = assigned program in
       List.dedup_and_sort result ~compare:String.compare

 
         

   
   let generate_cfg program =
     let nodes = blocks program in
     let nodesmap = blocks2map nodes in
     let edges = flow program in
     let pre_map = precessor_map nodes edges in
     let suc_map = successor_map nodes edges in
     {
      program = program;
      nodes = nodes;
      node_map = nodesmap;
      edges =  edges ;
      pre_map = pre_map;
      suc_map = suc_map;
     }

   
  
