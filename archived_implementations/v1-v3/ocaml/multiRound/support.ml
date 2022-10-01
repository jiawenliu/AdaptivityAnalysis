open List
open Distribution
open Printf
(* open Unix
 *)
let infile = ref (None : string option)
let outfile = ref (None : string option)
let cols = ref 1.0
let rows = ref 0.0 
let cdb = ref false
let mech_name = ref ""
let population_N = ref 0
let rounds = ref 0.0
let trails = ref 0

let argDefs = [
      "--createdb" , Arg.Unit (fun l -> cdb := true ), "create a new db";
      "-rw", Arg.Float (fun i -> rows := i), "specify the rows of the database, -rw float"; 
      "-r", Arg.Float (fun i -> rounds := i) , "specify the rounds (parameter k) of the multiRounds algorithm, -r int";
      "-t", Arg.Int (fun i -> trails := i) , "specify the trails of the experiments, -t int";
(*       "-db", Arg.String (fun s -> infile := Some s ), "specify the database file name, -i string" ; 
 *)   
      "-o", Arg.String (fun s -> outfile := Some s ), "specify the output file name, -o string";
      "-mech", Arg.String (fun s -> mech_name := s ), "specify the mechanism name, -mech string";
      "-N", Arg.Int (fun i -> population_N := i) , "specify the size of the population, -N int"
]
let delta = 0.0000001
let epsilon = 1.0

type row = float list
type database = row list
type query = row -> float
type mech = query -> database -> float
type result = float * float

let rec zip f a b = 
  match a, b with
  | xa::tla, xb::tlb -> (f xa xb) :: (zip f tla tlb)
  | _ -> []

let rec gen_dataset n = 
  if n = 0
  then []
else [float_of_int(n)] :: gen_dataset (n - 1)

let population = 
  ref (gen_dataset (!population_N) )

let parseArgs () =  
        let oname = ref (None : string option) in 
        Arg.parse argDefs 
        (fun s -> 
                match !oname  with 
                      | Some (_) -> printf "%s" "specify just one output file name"  
                      | None  -> oname := Some (s); printf "%s" s ) "for output file" ;
(*              match (!infile, !outfile)  with
                   |  Some i, Some o -> (i,o)
                   | _ -> printf "%s" "specify  your input file -i or output file -o , or colums -col float, rounds -r float"; ("", "")
 *)
              match (!outfile)  with
                   |  Some o -> (o)
                   | _ -> printf "%s" "specify  your input file -i or output file -o , or colums -col float, rounds -r float"; ""


let rec create_db_helper (col : float) (row : float)  =
  if row > 0.0
    then 
      let rec create_row (col : float) = 
        if col > 0.0
        then
          (float_of_int (Random.int (!population_N))) :: create_row (col -. 1.0)
        else
          []
      in (create_row col) :: create_db_helper col (row -. 1.0)
    else
      []

let create_db (col : float) (row : float)  =
        create_db_helper col row



let record_db db oc =
    (* let _ =  *)List.map (fun row -> let _ = List.map (fun e ->fprintf oc "%f," e ) row in fprintf oc "\n" )  db 
(*         in close_out oc *)

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



let dot (l1:float ) (l2: float) : float = l1 *. l2
  

let sign (y: float) : float = 
  if y >= 0.0 then 1.0 else -1.0

let get (row:float list) (i:float) : float  =
  List.nth row (int_of_float i)


let updt l pos a = 
  List.mapi (fun i x -> if i = (int_of_float pos) then a else x) l


let contains l i = 
  List.exists (fun a -> if (a = i) then true else false) l


let domain db = 
	let rec domain_without_duplicate db r = 
		match db with
		| [x] :: xs -> if contains r x 
						then domain_without_duplicate xs r 
						else domain_without_duplicate xs (x::r)
		| _ -> r
	in
	domain_without_duplicate db []

let restrict q seti =
  fun x -> 
    match x with
    | x::[] -> (* let _ =  fprintf stdout "data: %f " x in *)  if (contains seti x) 
    					then (* let _ = fprintf stdout " in selected set,\n " in *) 0.0 else let r = q x in (* let _ = fprintf stdout " not in selected set: %f\n " r in *) r
    | _ -> 0.0




let rec listminus l1 l2 = 
  match l2 with
    | hd::tl -> listminus (List.filter (fun a -> if (a = hd) then false else true) l1) tl
    | [] -> l1



let rec db_minus d l = 
	match d with
		| hd :: tl -> (listminus hd l) :: (db_minus tl l)
		| [] -> []


let rec mr_initial n =
	if n = 0.0 then
		[]
	else
	0.0 :: mr_initial (n -. 1.0)





let write res oc =
  fprintf oc "%f\n" res

let rec write_list res oc = 
  match res with
    | x::xs -> write x oc; write_list xs oc
    | [] -> fprintf oc "\n" 

let rec write_lists res oc = 
  match res with
  | x::xs -> write_list x oc; write_lists xs oc
  | [] -> fprintf oc "\n" 

