open Mechs 
open Support


let mech = gauss_mech

 let rec round (j ) = 
 (fun (db ) -> 
  (fun (k ) -> 
    if( (( j ) < ( k )) ) then 
       let a =
        mech (fun (x ) -> 
         (( ( ( get   x )   j ) ) *. ( ( ( get   x )   k ) )) 
       ) db  in
        ( a ,  j ) ::  ( ( ( round   (( j ) +. ( 1.000000 )) )   db )  
       k )  
    else 
      []
  )
 )
 
let g = round
 let rec twoRound (k ) = 
 (fun (db ) -> 
    let l =  ( ( ( g   0.000000 )   db )   k )  in
     let q =
     (fun (x ) -> 
       sign ( ( ( ( List.fold_left  (fun (acc ) -> 
                                     (fun (ai ) -> 
                                       (( acc ) +. ( (( ( ( get   x )  
                                       ( snd   ai ) ) ) *. ( log ( (( (( 1.000000 ) +. ( (
                                       fst   ai ) )) ) /. ( (( 1.000000 ) -. ( (
                                       fst   ai ) )) )) ) )) )) 
                                     )
                                    ))   0.000000 )   l ) ) 
     ) in
     mech  q  db 
 )
