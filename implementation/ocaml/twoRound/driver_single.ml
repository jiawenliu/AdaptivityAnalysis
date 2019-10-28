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

(*   let main  = 
    let (ifile, ofile) = parseArgs() in
    let oc = open_out ofile in 
    if !cdb 
    then
      let dbc = open_out ifile in
          let dataset = create_db !rows !cols in 
            let _ = experiments_tr 0 oc dataset in
            let _ = record_db dataset dbc in
            let _ =  close_out oc in
              close_out dbc
    else 
        let ic = open_in ifile in  
          let dataset = read_db ic !rows !cols in 
            let _ = experiments_tr 0 oc dataset in
              close_out oc;
              close_in ic
 *)



let main  = 
    let (ifile, ofile) = parseArgs() in
    let oc = open_out ofile in 
    let dataset =  
      if !cdb 
      then
        let dbc = open_out ifile in
          let data =  create_db !rows !cols in 
              let _ = record_db data dbc in
              data
      else 
          let ic = open_in ifile in  
            let data = read_db ic !rows !cols in
            let _ = close_in ic in
            data
    in 
          let result = experiments_tr 0 dataset 0.0 in
          let _ = write result oc in
              close_out oc
              
