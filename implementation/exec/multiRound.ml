open Printf 
open HeadFile 

let dataset = [ [1;1;1;1] ; [1;1;1;1] ; [1;1;1;1] ; [1;1;1;1] ]  

 
 let updtSC =
  let rec f ( z  ) = 
   fun ( sc  ) -> 
    fun ( a  ) -> 
     fun ( p  ) -> 
      fun ( q  ) -> 
       fun ( I  ) -> 
        fun ( i  ) -> 
         fun ( N  ) -> 
          if( (( i ) < ( N )) ) then 
            if(  contains    I   i   ) then 
               let x =
                ((  nth    sc   i   ) +. ( (( (( a ) -. ( p )) ) *. ( 
                q   (( i ) -. ( p ))  )) ))  in
                let sc' =   updt    sc    i   x     in
                 f    ()    sc'    a    p    q    I    (( i ) +. ( 1.000000 ))  
                                                      N         
            else 
               f    ()    sc    a    p    q    I    (( i ) +. ( 1.000000 ))  
                                                   N         
           
          else 
            sc 
         
        
       
      
     
    
   
  
  in
  let updtSCC =
   let rec f ( z  ) = 
    fun ( scc  ) -> 
     fun ( a  ) -> 
      fun ( p  ) -> 
       fun ( qc  ) -> 
        fun ( i  ) -> 
         fun ( C  ) -> 
          if( (( i ) < ( C )) ) then 
             let x =
              ((  nth    scc   i   ) +. ( (( (( a ) -. ( p )) ) *. ( 
              qc   (( i ) -. ( p ))  )) ))  in
              let scc' =   updt    scc    i   x     in
               f    ()    scc'    a    p    qc    (( i ) +. ( 1.000000 ))  
                                                 C        
          else 
            scc 
         
        
       
      
     
    
   
   in
   let updtI =
    let rec f ( z  ) = 
     fun ( maxSCC  ) -> 
      fun ( sc  ) -> 
       fun ( i  ) -> 
        fun ( N  ) -> 
         if( (( i ) < ( N )) ) then 
           if( ((  nth    scc   i   ) < ( maxScc )) ) then 
              i  ::   f    ()    maxScc    sc    (( i ) +. ( 1.000000 ))  
                                                N       
           else 
              f    ()    maxScc    sc    (( i ) +. ( 1.000000 ))   N      
          
         else 
           [] 
        
       
      
     
    
    in
   let rec f ( z  ) = 
      fun ( k  ) -> 
     fun ( j  ) -> 
      fun ( sc  ) -> 
       fun ( scc  ) -> 
        fun ( I  ) -> 
         fun ( N  ) -> 
          fun ( C  ) -> 
           fun ( D  ) -> 
            if( (( j ) < ( k )) ) then 
               let p =  new  in
                let q =  fun ( x  ) -> 
                          new 
                         in
                 let qc =  fun ( x  ) -> 
                            new 
                           in
                  let a =  mech( q )  in
                   let sc' =
                     updtSC    ()    sc    a    p    q    I    0.000000   N          in
                    let scc' =
                      updtSCC    ()    scc    a    p    qc    0.000000   C         in
                     let maxScc =
                       List.fold_left    fun ( acc  ) -> 
                                          fun ( a  ) -> 
                                           if( (( acc ) < ( a )) ) then 
                                             a 
                                           else 
                                             acc 
                                          
                                         
                                           0.000000   scc'     in
                      let I' =
                        updtI    ()    maxScc    sc    0.000000   N       in
                       let D' =  (( D )  ( I' ))  in
                        f      ()      k    (( j ) +. ( 1.000000 ))  
                                            sc'    scc'    I'    N   
                                                                     C  
                                                                     D'          
            else 
              D 
           
          
         
        
       
      
     
    
     
  