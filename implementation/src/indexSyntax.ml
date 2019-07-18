
(* ---------------------------------------------------------------------- *)
(* Abstract Syntax Tree for index terms                                   *)
(* ---------------------------------------------------------------------- *)


(* Sorts for index variables *)



type sort =
    Adapt




type var_info = {
  v_name  : string;
}

(* Index terms *)
type iterm =
  | IConst   	of int
  | IVar     	of var_info
  | IAdd     	of iterm * iterm
  | ISub   	  of iterm * iterm
  | IMaximal  of iterm * iterm


(* Map over index variables *)
let rec iterm_map f it =
  let smf = iterm_map f     in
  match it with
    IVar v           -> f v
  | IConst c         -> IConst c
  | IAdd(x, y)       -> IAdd (smf x, smf y)
  | ISub(x, y)       -> ISub (smf x, smf y)
  | IMaximal(i1, i2) -> IMaximal (smf i1, smf i2)


(* Substitution it[t/x] for index vars *)
let iterm_subst x t it =
  iterm_map (fun v -> if v = x then t else IVar v) it

let rec iterm_simpl it : iterm = 
  match it with
  | IAdd(it1, it2) ->
    let r1 = iterm_simpl it1 in
    let r2 = iterm_simpl it2 in
    begin 
      match r1,r2 with
      | IConst c1 , IConst c2 -> IConst (c1 + c2) 
      | IConst 0, _  -> r2
      | _, IConst 0  -> r1
      | _, _ ->  IAdd(r1, r2)
    end
  | ISub(it1, it2) ->     
    let r1 = iterm_simpl it1 in
    let r2 = iterm_simpl it2 in
    begin
      match r1,r2 with
      | _, IConst 0 -> r1
      | IConst c1 , IConst c2 -> IConst (c1 - c2) 
      | _, _ ->ISub(r1, r2)
    end
  | IMaximal(i1, i2) -> IMaximal (iterm_simpl i1, iterm_simpl i2)
  | IConst _ 
  | IVar _  -> it


let rec dedup (l: 'a list) : 'a list =
      match l with
        [] -> []
      | h :: tl -> let r = dedup tl in if List.mem h r then r else h :: r

(* Get free index variables ofan index term *)
let rec iterm_free_i_vars (it: iterm) : var_info list =
  match it with
    IVar v    ->  [v]
  | IConst c  -> []
  | IAdd (x, y) 
  | ISub  (x, y)  ->  dedup (iterm_free_i_vars x  @ iterm_free_i_vars y)
  | IMaximal (i1,i2) ->  dedup (iterm_free_i_vars i1  @ iterm_free_i_vars i2)

let add_adapts  sl sr : iterm =
  match  sl, sr with
    | IConst a, IConst b  -> IConst (a+b)
    | IConst 0, _         -> iterm_simpl sr
    | _, IConst 0         -> iterm_simpl sl
    | _                   -> IAdd(iterm_simpl sl, iterm_simpl sr)
        


let rec max_adapts q1 q2 : iterm =
  match q1, q2 with
    | IConst a, IConst b  -> 
        if (a > b) 
        then IConst a
        else IConst b
    | IConst 0, _         -> iterm_simpl q2
    | _, IConst 0         -> iterm_simpl q1
    | _                   -> IMaximal(iterm_simpl q1, iterm_simpl q2)

let rec adapt_subst z i witn : iterm =
  match z with
    | IVar j ->  
              if z = i 
              then witn
              else z
    | IAdd(j1, j2)      
      -> IAdd(adapt_subst j1 i witn, adapt_subst j2 i witn)
    | IMaximal(j1, j2)  
      -> IMaximal(adapt_subst j1 i witn, adapt_subst j2 i witn)
    | _ -> z

(* Depth Terms*)
type dterm =
  | DConst    of int
  | DVar      of var_info
  | DMaximal  of dterm * dterm
  | DAdd      of dterm * dterm
  | DSub      of dterm * dterm
  | DBot
  | DInfty



let add_depths d1 d2 =
  match d1,d2 with
  | DConst a, DConst b -> DConst (a + b)
  | DConst 0, _ -> d2
  | _, DConst 0 -> d1
  | _ -> DAdd(d1, d2)

let max_depths d1 d2 =
  match d1,d2 with
  | DConst a, DConst b -> 
    if (a > b)
    then 
      DConst a
    else
      DConst b
  | DBot, _ -> d2
  | _, DBot -> d1
  | _ -> DMaximal(d1, d2)
