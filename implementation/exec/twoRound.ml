open Printf 
open HeadFile 

let dataset = [ [1;1;1;1] ; [1;1;1;1] ; [1;1;1;1] ; [1;1;1;1] ]  

 let rec round ( j  ) = 
  fun ( k  ) -> 
    if( (( j ) < ( k )) ) then 
       let a =
        mech( fun ( x  ) -> 
         ((  get    x   j   ) *. (  get    x   k   )) 
       )  in
        ( a ,  j ) ::   round    (( j ) +. ( 1.000000 ))   k    
    else 
      [] 
     let rec twoRound ( k  ) = 
       let l =   round    0.000000   k    in
        let q =
         fun ( x  ) -> 
          sign (  List.fold_left (fun ( acc  ) -> 
                                               fun ( ( a ,  i ) ) -> 
                                                (( acc ) +. ( ((  get    x   i   ) *. ( log ( (( (( 1.000000 ) +. ( a )) ) /. ( (( 1.000000 ) -. ( a )) )) ) )) ))) 
                                    
                                      0.000000   l    ) 
         in
        mech( q ) 
     
 
