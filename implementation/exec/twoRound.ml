open HeadFile 

let dataset = [ [1;1;1;1] ; [1;1;1;1] ; [1;1;1;1] ; [1;1;1;1] ]  

 
 let g =
  let rec f ( j  ) = 
   fun ( k  ) -> 
    if( ( j )  <  ( k ) ) then 
       let a =  mech( fun ( x  ) -> 
                 (  x   j  )  dot  (  x   k  ) 
               )  in
        ( a ,  j ) ::   f    ( j )  +.  ( 1 )   k    
    else 
      [] 
   
   in f 
  in
  let l =   g    0   k    in
   let q =
    fun ( x  ) -> 
     sign (  List.fold_left   fun ( acc  ) -> 
                               fun ( ( a ,  i ) ) -> 
                                 ( acc )  +.  ( (  x   i  )  *.  ( log ( ( ( 1 )  +.  ( a ) )  /.  ( ( 1 )  -.  ( a ) ) ) ) )  
                                 0   l   
                              
                              ) 
    in