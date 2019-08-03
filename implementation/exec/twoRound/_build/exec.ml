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


  let main  = 
    let (ifile, ofile) = parseArgs() in

    let oc = open_out ofile in 
        let ic = open_in ifile in
          let dataset = read_db ic !rows !cols in 
            let _ = experiments_tr 0 oc dataset in
              close_out oc;
              close_in ic
