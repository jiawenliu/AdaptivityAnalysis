open Printf 
open HeadFile 

open Distribution 

let dataset = [ [1.0;1.0;1.0;1.0] ; [1.0;1.0;1.0;1.0] ; [1.0;1.0;1.0;1.0] ; [1.0;1.0;1.0;1.0] ]  

 let rec f ( z  ) = 
 (fun ( sc  ) -> 
  (fun ( a  ) -> 
   (fun ( p  ) -> 
    (fun ( q  ) -> 
     (fun ( ii  ) -> 
      (fun ( i  ) -> 
       (fun ( nn  ) -> 
         if( (( i ) < ( nn )) ) then 
           if((List.exists (fun a -> if (a =  i ) then true else false)  ii )) then 
              let x =
               ((  nth    sc   i   ) +. ( (( (( a ) -. ( p )) ) *. ( (( 
               q   i  ) -. ( p )) )) ))  in
               let sc' =   updt    sc    i   x     in
                f    ()    sc'    a    p    q    ii    (( i ) +. ( 1.000000 ))  
                                                      nn         
           else 
              f    ()    sc    a    p    q    ii    (( i ) +. ( 1.000000 ))  
                                                   nn         
          
         else 
           sc 
        
       )
      )
     )
    )
   )
  )
 )
 
let updtSC = f
 let rec f ( z  ) = 
 (fun ( scc  ) -> 
  (fun ( a  ) -> 
   (fun ( p  ) -> 
    (fun ( qc  ) -> 
     (fun ( i  ) -> 
      (fun ( cc  ) -> 
        if( (( i ) < ( cc )) ) then 
           let x =
            ((  nth    scc   i   ) +. ( (( (( a ) -. ( p )) ) *. ( (( 
            qc   i  ) -. ( p )) )) ))  in
            let scc' =   updt    scc    i   x     in
             f    ()    scc'    a    p    qc    (( i ) +. ( 1.000000 ))  
                                               cc        
        else 
          scc 
       
      )
     )
    )
   )
  )
 )
 
let updtSCC = f
 let rec f ( z  ) = 
 (fun ( maxScc  ) -> 
  (fun ( sc  ) -> 
   (fun ( i  ) -> 
    (fun ( nn  ) -> 
      if( (( i ) < ( nn )) ) then 
        if( ((  nth    sc   i   ) < ( maxScc )) ) then 
           i  ::   f    ()    maxScc    sc    (( i ) +. ( 1.000000 ))   nn       
        else 
           f    ()    maxScc    sc    (( i ) +. ( 1.000000 ))   nn      
       
      else 
        [] 
     
    )
   )
  )
 )
 
let updtI = f
 let rec multiRound ( z  ) = 
   (fun ( k  ) -> 
  (fun ( j  ) -> 
   (fun ( sc  ) -> 
    (fun ( scc  ) -> 
     (fun ( ii  ) -> 
      (fun ( nn  ) -> 
       (fun ( cc  ) -> 
        (fun ( dd  ) -> 
          if( (( j ) < ( k )) ) then 
             let p = (sample_uniform  0.000000   1.000000 ) in
              let q = (fun ( x  ) -> 
                       (sample_bernoulli( p ))
                      ) in
               let qc = (fun ( x  ) -> 
                         (sample_bernoulli( p ))
                        ) in
                let a =  mech( q )  in
                 let sc' =
                   updtSC    ()    sc    a    p    q    ii    0.000000   nn          in
                  let scc' =
                    updtSCC    ()    scc    a    p    qc    0.000000   cc         in
                   let maxScc =
                     List.fold_left   (fun ( acc  ) -> 
                                       (fun ( a  ) -> 
                                         if( (( acc ) < ( a )) ) then 
                                           a 
                                         else 
                                           acc 
                                        
                                       )
                                      )   0.000000   scc'     in
                    let ii' =
                      updtI    ()    maxScc    sc    0.000000   nn       in
                     let dd' = (listminus  dd   ii' ) in
                      multiRound      ()      k    (( j ) +. ( 1.000000 ))  
                                                   sc'    scc'    ii'  
                                                                  nn  
                                                                  cc  
                                                                 dd'          
          else 
            dd 
         
        )
       )
      )
     )
    )
   )
  )
 )  
