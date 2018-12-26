
 
type 'a bang  = 
    Bang of 'a 
type database = int -> int list
type query = int -> int list -> int
type mech = query -> database -> float
type result = float * int

let rec dot (l1:int list) (l2: int list) : int =
  match l1 , l2 with 
  | x :: tl1, y ::tl2 -> x*y + (dot tl1 tl2)
  | _ ,_ -> 0

let sign (y: float) : int = 
  if y >. 0.0 then 1 else -1

let g (m:mech) (k:int) (d1:database) : result list = 
  let rec f (m:mech) (j:int) (k:int) (d1:database) =
     if j < k then
       let a = m (fun d -> dot (d j) (d k) ) (d1) in
           (a, j):: (f m j+1 k d1) 
     else []
   in
  f m 0 k d1


let two_round d k m = 
 let l = g m k d in 
  let q = fun d -> 
     sign ( List.fold_left  
            (fun a r ->
              let x = 1.0+.(fst r) /. (1.0 -. fst r ) in 
                let y = (float_of_int List.hd (d (snd r) )) +. log x in 
                   a +. y  
            ) 0.0 l
          )
   in 
  (m q d)
       


            
 
