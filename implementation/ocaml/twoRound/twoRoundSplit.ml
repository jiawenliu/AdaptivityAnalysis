open Mechs
open Support

let mech = nonoise_mech

 let rec round j db k = 

    if( (( j ) < ( k )) ) then 
       let a =
        mech (fun (x ) -> 
         (( ( ( get   x )   j ) ) *. ( ( ( get   x )   k ) )) 
       ) db  in
        ( a ,  j ) ::  ( ( ( round   (( j ) +. ( 1.000000 )) )   db )  
       k )  
    else 
      []
 
let g = round
 let rec twoRound (k ) db1 db2 = 
    let l =  ( ( ( g   0.000000 )   db1 )   k )  in
     let q =
     (fun (x ) -> (get x k) *.
       sign ( ( ( ( List.fold_left  (fun (acc ) -> 
                                     (fun (ai ) -> 
                                       (( acc ) +. ( (( ( ( get   x )  
                                       ( snd   ai ) ) ) *. ( log ( (( (( 1.000000 ) +. ( (
                                       fst   ai ) )) ) /. ( (( 1.000000 ) -. ( (
                                       fst   ai ) )) )) ) )) )) 
                                     )
                                    ))   0.000000 )   l ) ) 
     ) in
     mech  q  db2 
