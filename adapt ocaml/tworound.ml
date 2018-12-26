open List
open Distribution
 
type 'a bang  = 
    Bang of 'a 
type database = int list list
type query = database -> int
type mech = query -> database -> float
type result = float * int



let dataset = [[1;1;1;1] ; [1;1;1;1] ; [1;1;1;1] ;[1;1;1;1] ] 

let sum_q (db: int list list) = 
    List.fold_left ( fun a r -> a +  (hd r)  ) 0 db  

let gauss_mech query db =  
    (query db) +. (sample_normal_boxmuller3 0.0 1.0 )

let rec dot (l1:int list) (l2: int list) : int =
  match l1 , l2 with 
  | x :: tl1, y ::tl2 -> x*y + (dot tl1 tl2)
  | _ ,_ -> 0

let sign (y: float) : int = 
  if y > 0.0 then 1 else -1

let get (d:database) (i:int) : int list =
  nth d i

let g (m:mech) (k:int) (d1:database) : result list = 
  let rec f (m:mech) (j:int) (k:int) (d1:database) =
     if j < k then
       let a = m (fun d -> dot (get d j) (get d k) ) (d1) in
           (a, j):: (f m (j+1) k d1)
     else []
   in
  f m 0 k d1


let two_round d k m = 
 let l = g m k d in 
  let q = fun d -> 
     sign ( List.fold_left  
            (fun a r ->
              let x = 1.0+.(fst r) /. (1.0 -. fst r ) in 
                let y = float_of_int  ( List.hd (get d (snd r) )) +. log x in 
                   a +. y  
            ) 0.0 l
          )
   in 
  (m q d)

  let main  = two_round dataset 4 gauss_mech
       


            
 
