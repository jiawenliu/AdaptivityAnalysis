open Printf 
open Support
open Mechs 

open Distribution 

(* let population = Support.gen_dataset (!population_N) *) 

 let rec updtSC (z ) = 
 (fun (sc ) -> 
  (fun (a ) -> 
   (fun (p ) -> 
    (fun (q ) -> 
     (fun (ii ) -> 
      (fun (i ) -> 
       (fun (nn ) -> 
         if( (( i ) < ( nn )) ) then 
           if( ( ( contains   ii )   i ) ) then 
             ( ( ( ( ( ( ( ( updtSC   () )   sc )   a )   p )   q )   ii )  
               (( i ) +. ( 1.000000 )) )   nn ) 
           else 
              let x =
               (( ( ( get   sc )   i ) ) +. ( (( (( a ) -. ( p )) ) *. ( (( (
               q   i ) ) -. ( p )) )) ))  in
               let sc' =  ( ( ( updt   sc )   i )   x )  in
               ( ( ( ( ( ( ( ( updtSC   () )   sc' )   a )   p )   q )   ii )  
                 (( i ) +. ( 1.000000 )) )   nn ) 
          
         else 
           sc 
        
       )
      )
     )
    )
   )
  )
 )
 

let rec updtSCC (z ) = 
 (fun (scc ) -> 
  (fun (a ) -> 
   (fun (p ) -> 
    (fun (qc ) -> 
     (fun (i ) -> 
      (fun (cc ) -> 
        if( (( i ) < ( cc )) ) then 
           let x =
            (( ( ( get   scc )   i ) ) +. ( (( (( a ) -. ( p )) ) *. ( (( (
            qc   i ) ) -. ( p )) )) ))  in
            let scc' =  ( ( ( updt   scc )   i )   x )  in
            ( ( ( ( ( ( ( updtSCC   () )   scc' )   a )   p )   qc )   (( i ) +. ( 1.000000 )) )  
            cc ) 
        else 
          scc 
       
      )
     )
    )
   )
  )
 )
 

let rec updtI (z ) = 
 (fun (maxScc ) -> 
  (fun (sc ) -> 
   (fun (i ) -> 
    (fun (nn ) ->
    fun ii db -> 
      if( (( i ) < ( nn ))  ) then 
        if( (( ( ( get   sc )   i ) ) > ( maxScc )) && (not (contains ii i)) && (contains db i)) then 
           i  ::  ( ( ( ( ( updtI   () )   maxScc )   sc )   (( i ) +. ( 1.000000 )) )  
          nn ii db)  
        else 
          ( ( ( ( ( updtI   () )   maxScc )   sc )   (( i ) +. ( 1.000000 )) )  
          nn ii db) 
       
      else 
        ii 
     
    )
   )
  )
 )
 

 let rec multiRound (z ) = 
   (fun (k ) -> 
  (fun (j ) -> 
   (fun (sc ) -> 
    (fun (scc ) -> 
     (fun (ii ) -> 
      (fun (nn ) -> 
       (fun (cc ) -> 
        (fun (db ) dom -> 
          if( (( j ) < ( k )) ) then 
             let p = (sample_uniform  0.000000   1.000000 ) in
              let q = (fun (x ) -> 
                       (sample_bernoulli( p ))
                      ) in
               let qc = (fun (x ) -> 
                         (sample_bernoulli( p ))
                        ) in
                let qj =  ( ( restrict   q )   ii )  in
                 let a =  mech  qj  db  in
                  let true_v = nonoise_mech qj (Support.gen_dataset (!population_N)) in 
                  let sc' =
                   ( ( ( ( ( ( ( ( updtSC   () )   sc )   a )   p )   q )  
                       ii )   0.000000 )   nn )  in
                   let scc' =
                    ( ( ( ( ( ( ( updtSCC   () )   scc )   a )   p )   qc )  
                      0.000000 )   cc )  in
                    let maxScc =
                     ( ( ( List.fold_left  (fun (acc ) -> 
                                            (fun (a ) -> 
                                              if( (( acc ) < ( a )) ) then 
                                                a 
                                              else 
                                                acc 
                                             
                                            )
                                           ))   (-10.000000) )   scc' )  in
                     let ii' =
                      (( ( ( ( ( updtI   () )   maxScc )   sc )   0.000000 )  
                                              nn ) ii dom)  in
                       (* let _ = write_list ii' stdout in *)
                       (a, true_v)  ::  ( ( ( ( ( ( ( (   ( multiRound   () )     k )  
                                          (( j ) +. ( 1.000000 )) )  
                                        sc' )   scc' )   ii' )   nn )  
                                cc )   db dom)  
          else 
            [] 
         
        )
       )
      )
     )
    )
   )
  )
 )  
