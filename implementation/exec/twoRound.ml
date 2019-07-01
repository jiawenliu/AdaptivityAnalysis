open Printf 
open HeadFile 


 let rec round ( j  ) db = 
 (fun ( k  ) -> 
   if( (( j ) < ( k )) ) then 
      let a =
       mech(fun ( x  ) -> 
        ((  get    x   j   ) *. (  get    x   k   )) 
      )  db in
       ( a ,  j ) ::   round    (( j ) +. ( 1.000000 )) db   k    
   else 
     [] 
  
 )
 
 let g = round 
  let rec twoRound k db = 
   let l =   g    0.000000 db  k    in
    let q =
    (fun ( x  ) -> 
      sign (  List.fold_left   (fun ( acc  ) -> 
                                (fun ( ( a ,  i ) ) -> 
                                  (( acc ) +. ( ((  get    x   i   ) *. ( log ( (( (( 1.000000 ) +. ( a )) ) /. ( (( 1.000000 ) -. ( a )) )) ) )) )) 
                                )
                               )   0.000000   l    ) 
    ) in
    mech q db 
