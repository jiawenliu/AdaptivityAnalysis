open MultiRound
open Printf 
open HeadFile




let write res oc =
    fprintf oc "%f\n" res

let rec write_list res oc = 
  match res with
    | x::xs -> write x oc; write_list xs oc
    | [] -> ()


  let rec experiments_mr r n k oc dataset  =
     if r < !rounds then
      let c = n in
        let sc = mr_initial n in
          let scc = mr_initial n in
        let x = MultiRound.multiRound () k 0.0 sc scc [] n c dataset in
        write_list (x) oc ; 
        experiments_mr (r+1) n k oc dataset
      else ()

  let main  = 
    let (ifile, ofile) = parseArgs() in

    let oc = open_out ofile in 
        let ic = open_in ifile in
          let dataset = read_db ic !rows !cols in 
            let _ = experiments_mr 0 !n !k oc dataset in
              close_out oc;
              close_in ic
