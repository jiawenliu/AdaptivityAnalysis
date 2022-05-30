open Syntax

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

  (* Control flow graph*)

  let rec flow lcom = 
    match lcom with
    |  Skip  -> []
    | Assign ( _ , _ , _) -> []
    | Query ( _ ,  _ , _ ) -> []
    | While ( _ , lc , l ) ->   (flow lc) @ [(l, init lc)] @ (List.map (fun l_1 -> (l_1,l)) (final lc) ) 
    | Seq ( lc_1,  lc_2 ) -> (flow lc_1) @ (flow lc_2) @ (List.map (fun l_1 -> (l_1, init lc_2 )) (final lc_1) )
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


   (*  Reaching Definition**)  
    
    
    




   
   (* 
    To do: need RD first
   let rec cdfg lcom  : lvar * lvar list = 
    match lcom with
    |  Skip  -> []
    | Assign ( x , e , l) -> [  ]  (vars e) 
    | Query ( _ ,  _ , _ ) -> []
    | While ( _ , lc , l ) ->   (flow lc) @ [(l, init lc)] @ (List.map (fun l_1 -> (l_1,l)) (final lc) ) 
    | Seq ( lc_1,  lc_2 ) -> (flow lc_1) @ (flow lc_2) @ (List.map (fun l_1 -> (l_1, init lc_2 )) (final lc_1) )
    | If ( _ , lc_1 , lc_2 , l ) -> [ ( l, init lc_1 ) ; (l, init lc_2) ] @ (flow lc_1) @ (flow lc_2)  *)
