open TwoRound
open Printf 
open HeadFile



  let write res oc =
    fprintf oc "%f\n" res

let rec write_list res oc = 
  match res with
    | x::xs -> write x oc; write_list xs oc
    | [] -> ()

  let rec experiments_tr r oc dataset =
     if r < !rounds then
        let x = TwoRound.twoRound 6.0 dataset in
        write (x) oc ; experiments_tr (r+1) oc dataset
      else close_out oc

  let rec experiments_tr r dataset result =
     if r < !rounds then
        let x = TwoRound.twoRound 6.0 dataset in
        experiments_tr (r+1) dataset (result +. x)
      else result /. (float_of_int !rounds )


let experimet_for_one_col ifile oc colnum =
    let dataset =  
      if (!cdb) 
      then
        let dbc = open_out ifile in
          let data =  create_db !rows colnum in 
              let _ = record_db data dbc in
              data
      else 
          let ic = open_in ifile in  
            let data = read_db ic !rows colnum in
            let _ = close_in ic in
            data
    in 
          let result = experiments_tr 0 dataset 0.0 in
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

              
