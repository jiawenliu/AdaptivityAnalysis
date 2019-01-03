open List
open Distribution
open Printf

let infile = ref (None : string option)
let outfile = ref (None : string option)
let rounds = ref 0
let cols = ref 0
let rows = ref 0 

let argDefs = [
    "-rw", Arg.Int (fun i -> rows := i) , "specify the rows of the database, -rw int"; 
    "-cl", Arg.Int (fun i -> cols := i) , "specify the cols of the database, -cl int"; 
    "-r", Arg.Int (fun i -> rounds := i) , "specify the rounds of the experiments, -r int"; 
    "-i", Arg.String (fun s -> infile := Some s ), "specify the input file name, -i string" ; 
      "-o", Arg.String (fun s -> outfile := Some s ), "specify the output file name, -o string" 
]
let delta = 0.0000001
let epsilon = 1.0

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
                   | _,_ -> printf "%s" "specify  your input file -i or output file -o , or colums -col int, rounds -r int"; ("","")

let dataset = [ [1;1;1;1] ; [1;1;1;1] ; [1;1;1;1] ; [1;1;1;1] ] 


let rec creat_db (col : int) (row : int)  =
  if row > 0 
    then 
      let rec creat_row (col : int) = 
        if col > 0
        then 
          (Random.int 2) :: creat_row (col - 1)
        else
          []
      in (creat_row col) :: creat_db col (row - 1)
    else
      []


let thresholdout_mech (q:query) db =
  let holdout_db = creat_db (List.length (hd db)) (List.length db) in
    let threshold = 1.0 in
      let noise_rate = 1.0 in
        let budget = (List.length db) in
              let rec threshold_mech db holdout_db gamma =
                match (db, holdout_db) with
                  |([],[]) -> []
                  |(rw::t, holdout_rw::holdout_t) -> let eta = (sample_exp (4.0 *. noise_rate)) in
                    if ((q rw) -. (q holdout_rw)) > (threshold +. gamma +. eta)
                    then
                      ((q holdout_rw) +. (sample_exp noise_rate))::(threshold_mech t holdout_t (sample_exp 2.0 *. noise_rate))
                    else
                      (q rw)::(threshold_mech t holdout_t gamma)
                  | _ -> printf "This shouldn't happen\n";[]
                  in
                    let sm = List.fold_left 
                      (fun sum rw ->  
                          sum +. rw
                      ) 0.0 (threshold_mech db holdout_db (sample_exp (2.0 *. noise_rate)))
                    in 
                      (sm /.  float_of_int (List.length (hd db)))


(*let rec read_rows ic i j acc =
        if i < j then
         read_line ic (i+1) j (int_of_string (input_line ic)) :: acc
        else
         List.rev acc *)
       

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
        let mu = 2.0 *. log(1.25 /. delta) *. 2.0 *. 2.0 /. epsilon
        in 
          mean +. (sample_normal_boxmuller3 0.0 mu) 
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
        let x = two_round dataset 4 thresholdout_mech in
        let y =  two_round dataset 4 nonoise_mech in
        write (x-.y) oc ; experiments oc (i+1)
      else close_out oc

  let main  = 
   let (ifile,ofile) = parseArgs() in
    let oc = open_out ofile in 
     experiments oc 0
        
       
  

            
 
