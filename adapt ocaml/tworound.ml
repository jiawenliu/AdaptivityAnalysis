open List
open Distribution
open Printf

let infile = ref (None : string option)
let outfile = ref (None : string option)
let rounds = ref 0

let argDefs = [
    "-r", Arg.Int (fun i -> rounds := i) , "specify the rounds of the experiments"; 
    "-i", Arg.String (fun s -> infile := Some s ), "specify the input file name" ; 
      "-o", Arg.String (fun s -> outfile := Some s ), "specify the output file name" 
]

type 'a bang  = 
    Bang of 'a 
type row = int list
type database = row list
type query = row -> float
type mech = query -> database -> float
type result = float * int

let parseArgs () =  
        let oname = ref (None : string option) in 
        Arg.parse argDefs 
        (fun s -> 
                match !oname  with 
                      | Some (_) -> printf "%s" "specify just one output file name"  
                      | None  -> oname := Some (s); printf "%s" s ) "for output file" ;
             match !infile, !outfile  with
                   | Some i, Some o -> (i,o)
                   | _,_ -> printf "%s" "specify  your input file or output file"; ("","")

let dataset = [[1;1;1;1] ; [1;1;1;1] ; [1;1;1;1] ;[1;1;1;1] ] 

let sum_q (db: int list list) = 
    List.fold_left ( fun a r -> a +  (hd r)  ) 0 db  

let gauss_mech (q:query) db =  
  let result =
    let mean = 
      let sm =  List.fold_left 
          ( fun sum rw ->  
              sum +. (q rw)
          ) 0.0 db  
        in  
        sm /.  float_of_int (List.length db)
      in
    mean +. (sample_normal_boxmuller3 0.0 1.0 ) 
  in 
  if result > 1.0 then 1.0 
  else  if result < -1.0 then -1.0 else result 

let nonoise_mech (q:query) db =  
    let mean = 
      let sm =  List.fold_left 
          ( fun sum rw ->  
              sum +. (q rw)
          ) 0.0 db  
        in  
        sm /.  float_of_int (List.length db)
      in
    mean 

let dot (l1:int ) (l2: int) : int = l1 * l2
  

let sign (y: float) : float = 
  if y > 0.0 then 1.0 else -1.0

let get (row:int list) (i:int) : int  =
  nth row i

let round (m:mech) (k:int) (d1:database) : result list = 
  let rec f (m:mech) (j:int) (k:int) (d1:database) =
     if j < k then
       let a = m 
       (fun row -> float_of_int ( dot (get row j) (get row (k-1) ) ) )
        d1 in
           (a, j):: (f m (j+1) k d1)
     else []
   in
  f m 0 k d1


let two_round d k m = 
 let l = round m k d in 
  let q = fun rw -> 
     sign ( List.fold_left  
            (fun a r ->
              let x = 1.0+.(fst r) /. (1.0 -. fst r ) in 
                let y = float_of_int  ( get rw (snd r) ) *. log x in 
                   a +. y  
            ) 0.0 l
          )
      (* (float_of_int sm) /.  float_of_int (List.length d) *)
     (* sign ( List.fold_left  
            (fun a r ->
              let x = 1.0+.(fst r) /. (1.0 -. fst r ) in 
                let y = float_of_int  ( List.hd (get d (snd r) )) +. log x in 
                   a +. y  
            ) 0.0 l
          ) *)
   in 
  (m q d)

  let write res oc =
    fprintf oc "%f\n" res

  let rec experiments oc i =
     if i < !rounds then
        let x = two_round dataset 4 gauss_mech in
        let y =  two_round dataset 4 nonoise_mech in
        write (x-.y) oc ; experiments oc (i+1)
      else close_out oc

  let main  = 
   let (ifile,ofile) = parseArgs() in
    let oc = open_out ofile in 
     experiments oc 0
        
       
  

            
 
