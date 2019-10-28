open TwoRound
open Printf 
open HeadFile
open TwoRoundSplit



  let write res oc =
    fprintf oc "%f\n" res

let rec write_list res oc = 
  match res with
    | x::xs -> write x oc; write_list xs oc
    | [] -> ()

(*   let rec experiments_tr r oc dataset =
     if r < !rounds then
        let x = TwoRound.twoRound 6.0 dataset in
        write (x) oc ; experiments_tr (r+1) oc dataset
      else close_out oc *)

  let rec experiments_tr r db result k =
     if r < !rounds then
        let x = TwoRound.twoRound k db in
        experiments_tr (r+1) db (result +. x) k
      else result /. (float_of_int !rounds )

  let rec experiments_tr_split r db1 db2 result k =
     if r < !rounds then
        let x = TwoRoundSplit.twoRound k db1 db2 in
        experiments_tr (r+1) db1 db2 (result +. x) k
      else result /. (float_of_int !rounds )

let experimet_for_one_col ifile oc colnum =
  if (!mech_name = "split") then
  let (db1, db2) =  (create_db !rows colnum, create_db !rows colnum)
    in 
      let result = experiments_tr_split 0 db1 db2 0.0 colnum in
        write result oc 

else
    let db =  
      if (!cdb) 
      then
          create_db !rows colnum
      else 
          let ic = open_in ifile in  
            let data = read_db ic !rows colnum in
            let _ = close_in ic in
            data
    in 
          let result = experiments_tr 0 db 0.0 colnum in
          write result oc

let rec experiments_for_cols colnum oc =
 if colnum < !cols
 then 
  let _ = experimet_for_one_col ("datas/data"^string_of_int(int_of_float(colnum))^".txt") oc (colnum)
  in 
    let coln = if colnum < 100.0
      then colnum +. 10.0
      else if colnum < 1000.0
      then  colnum +. 100.0
      else colnum +. 1000.0
    in
      experiments_for_cols coln oc
else
  close_out oc


let main  = 
    let (ifile, ofile) = parseArgs() in
    let oc = open_out ofile in
    experiments_for_cols 10.0 oc

              
