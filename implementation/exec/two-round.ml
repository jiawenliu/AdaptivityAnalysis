  let g =
   let rec f ( j  ) = 
    fun ( k  ) -> 
     if( ( j   <   k ) ) then 
        let a =  mech(  fun ( x  ) -> 
                  (  x   j    *    x   k  ) 
                 ) in
         ( a ,  j ) ::   f    ( j   +   1 )   k    
     else 
       [] 
    
    in f 
   in
   let l =   g    0   K    in
    let q =
     fun ( x  ) -> 
      sign (   foldl   fun ( acc  ) -> 
                        fun ( ( a ,  i ) ) -> 
                          ( acc   +   (  x   i    *   log (  ( ( 1   +   a )   /  
                                                      ( 1   -   a ) )  ) ) )  
                          0   l   
                       
                        ) 
     in