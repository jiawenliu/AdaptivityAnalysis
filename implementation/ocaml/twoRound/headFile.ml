open List
open Distribution
open Printf

let infile = ref (None : string option)
let outfile = ref (None : string option)
let rounds = ref 0
let cols = ref 0.0
let rows = ref 0.0 
let cdb = ref false
let mech_name = ref ""
let colst = ref 10.0
let argDefs = [
      "--createdb" , Arg.Unit (fun l -> cdb := true ), "create a new db";
      "-rw", Arg.Float (fun i -> rows := i) , "specify the rows of the database, -rw float"; 
      "-cl", Arg.Float (fun i -> cols := i) , "specify the cols of the database, -cl float"; 
      "--clst", Arg.Float (fun i -> colst := i) , "specify the start numbers of cols of the database, -cl float"; 
      "-r", Arg.Int (fun i -> rounds := i) , "specify the rounds of the experiments, -r int"; 
      "-db", Arg.String (fun s -> infile := Some "datas/data.txt" ), "specify the database file name, -i string" ; 
      "-o", Arg.String (fun s -> outfile := Some s ), "specify the output file name, -o string";
      "--mech", Arg.String (fun s -> mech_name := s ), "specify the mechanism name, -mech string"  
]
let delta = 0.0000001
let epsilon = 1.0

type row = float list
type database = row list
type query = row -> float
type mech = query -> database -> float
type result = float * float

let parseArgs () =  
        let oname = ref (None : string option) in 
        Arg.parse argDefs 
        (fun s -> 
                match !oname  with 
                      | Some (_) -> printf "%s" "specify just one output file name"  
                      | None  -> oname := Some (s); printf "%s" s ) "for output file" ;
             match (!infile, !outfile)  with
                   |  Some i, Some o -> (i,o)
                   | _ -> printf "%s" "specify  your input file -i or output file -o , or colums -col float, rounds -r float"; ("", "")

  

let rec create_db (col : float) (row : float)  =
  if row > 0.0
    then 
      let rec create_row (col : float) = 
        if col > 0.0
        then 
          if (Random.int 2) == 0
          then
            (- 1.0 ) :: create_row (col -. 1.0)
          else
            (1.0 ) :: create_row (col -. 1.0)
        else
          []
      in (create_row col) :: create_db col (row -. 1.0)
    else
      []

let record_db db oc =
    List.map (fun row -> List.map (fun e ->fprintf oc "%f," e ) row; fprintf oc "\n" )  db ; close_out oc

let sub_row row =
  List.rev (List.tl (List.rev row) )


let rec read_db ic rows cols =
    if rows > 0.0
     then 
          let line = input_line ic in 
          let tmp_row =
           (String.split_on_char ',' line) in 
           (List.map (fun s -> float_of_string s) (sub_row tmp_row)) :: (read_db ic (rows-.1.0) cols)
    else
       [] 


let thresholdout_mech (q:query) db =
  let holdout_db = create_db (float_of_int (List.length (hd db))) (float_of_int(List.length db)) in
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
         read_line ic (i+1) j (float_of_string (input_line ic)) :: acc
        else
         List.rev acc *)
       

let sum_q (db: float list list) = 
    List.fold_left ( fun a r -> a +.  (hd r)  ) 0.0 db  

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
(*       let _ = print_endline(string_of_int(List.length db) ^ (string_of_int(List.length (List.nth db 0)))) in
 *)        let mu = ( sqrt( (sqrt (float_of_int (List.length (List.nth db 0))) )   /. (float_of_int (List.length db)) ))(*2.0 *. log(1.25 /. delta) *. 2.0 *. 2.0 /. epsilon*)
        in 
          mean +. (sample_normal_boxmuller3 0.0 mu) 
  in 
  if result > 1.0 then 1.0 
  else  if result < -1.0 then -1.0 else result 

let nonoise_mech (q:query) db =  
    let mean = 
      let sm =  List.fold_left 
          ( fun sum rw ->  
              sum +.  (q rw)
          ) 0.0 db  
        in  
        sm /.  float_of_int (List.length db)
      in
    mean 

let restrict q db =
  fun x -> 
    match x with
      [] -> 0.0
    | _ -> q x

let dot (l1:float ) (l2: float) : float = l1 *. l2
  

let sign (y: float) : float = 
  if y >= 0.0 then 1.0 else -1.0

let get (row:float list) (i:float) : float  =
  List.nth row (int_of_float i)


let updt l pos a = 
  List.mapi (fun i x -> if i = (int_of_float pos) then a else x) l




let rec listminus l1 l2 = 
  match l2 with
    | hd::tl -> listminus (List.filter (fun a -> if (a = hd) then false else true) l1) tl
    | [] -> l1


let contains l i = 
  List.exists (fun a -> if (a = i) then true else false) l

let rec db_minus d l = 
	match d with
		| hd :: tl -> (listminus hd l) :: (db_minus tl l)
		| [] -> []


let rec mr_initial n =
	if n = 0.0 then
		[]
	else
	0.0 :: mr_initial (n -. 1.0)

(*let dataset = 
   [[1.0;1.0;0.0;1.0;0.0;0.0;1.0];
	[1.0;1.0;0.0;1.0;0.0;0.0;1.0];
	[1.0;1.0;0.0;1.0;0.0;0.0;1.0];
	[1.0;1.0;0.0;1.0;0.0;0.0;1.0];
	[1.0;1.0;0.0;1.0;0.0;0.0;1.0];
	[1.0;1.0;0.0;1.0;0.0;0.0;1.0];
	[1.0;1.0;0.0;1.0;0.0;0.0;1.0]]
*)
(*let dataset = 
	[[1.];
	 [2.];
	 [2.];
	 [1.];
	 [4.];
	 [5.];
	 [8.]]
*)


