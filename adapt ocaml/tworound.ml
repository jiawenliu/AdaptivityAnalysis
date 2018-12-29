open List
open Distribution
open Printf


type 'a bang  = 
    Bang of 'a 
type database = int list list
type query = database -> float
type mech = query -> database -> float
type result = float * int



let dataset = [[1;1;1;1] ; [1;1;1;1] ; [1;1;1;1] ;[1;1;1;1] ] 

let sum_q (db: int list list) = 
    List.fold_left ( fun a r -> a +  (hd r)  ) 0 db  

let gauss_mech (q:query) db =  
    (q db) +. (sample_normal_boxmuller3 0.0 1.0 )

let nonoise_mech (q:query) db =  
    (q db) 

let dot (l1:int ) (l2: int) : int = l1 * l2
  

let sign (y: float) : int = 
  if y > 0.0 then 1 else -1

let get (row:int list) (i:int) : int  =
  nth row i

let g (m:mech) (k:int) (d1:database) : result list = 
  let rec f (m:mech) (j:int) (k:int) (d1:database) =
     if j < k then
       let a = m 
       (fun d ->
          let sm =  List.fold_left 
          ( fun sum row ->  
              sum + dot (get row j) (get row (k-1) )
          ) 0 d  in  
            (float_of_int sm) /.  float_of_int (List.length d)
           )
        d1 in
           (a, j):: (f m (j+1) k d1)
     else []
   in
  f m 0 k d1


let two_round d k m = 
 let l = g m k d in 
  let q = fun d -> 
   let sm =  List.fold_left ( fun sum row -> 
     sign ( List.fold_left  
            (fun a r ->
              let x = 1.0+.(fst r) /. (1.0 -. fst r ) in 
                let y = float_of_int  ( get row (snd r) ) +. log x in 
                   a +. y  
            ) 0.0 l
          )
   )
      0 d in
      (float_of_int sm) /.  float_of_int (List.length d)
     (* sign ( List.fold_left  
            (fun a r ->
              let x = 1.0+.(fst r) /. (1.0 -. fst r ) in 
                let y = float_of_int  ( List.hd (get d (snd r) )) +. log x in 
                   a +. y  
            ) 0.0 l
          ) *)
   in 
  (m q d)

  let main  = 
      let x = two_round dataset 4 gauss_mech in
        let y =  two_round dataset 4 nonoise_mech in
          let z = two_round dataset 4 gauss_mech in
        printf "gauss_mech:%f, no noise mech is: %f, gauss2 : %f \n" x y z
       


            
 
