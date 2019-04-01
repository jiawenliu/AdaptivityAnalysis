(******************************************************************************)
(*     Alt-Ergo: The SMT Solver For Software Verification                     *)
(*     Copyright (C) 2013-2014 --- OCamlPro                                   *)
(*     This file is distributed under the terms of the CeCILL-C licence       *)
(******************************************************************************)

(******************************************************************************)
(*     The Alt-Ergo theorem prover                                            *)
(*     Copyright (C) 2006-2013                                                *)
(*     CNRS - INRIA - Universite Paris Sud                                    *)
(*                                                                            *)
(*     Sylvain Conchon                                                        *)
(*     Evelyne Contejean                                                      *)
(*                                                                            *)
(*     Francois Bobot                                                         *)
(*     Mohamed Iguernelala                                                    *)
(*     Stephane Lescuyer                                                      *)
(*     Alain Mebsout                                                          *)
(*                                                                            *)
(*   This file is distributed under the terms of the CeCILL-C licence         *)
(******************************************************************************)

module Z1 = struct
  include Big_int

  type t = big_int

  let zero = zero_big_int
  let one = unit_big_int
  let my_gcd a b = gcd_big_int a b
  let my_lcm a b = div_big_int (mult_big_int a b) (my_gcd a b)
  let of_string = big_int_of_string

end

module Q1 = struct

  exception Not_a_float

  include Num

  type t = num

  let zero = Int 0
  let one = Int 1
  let m_one = Int (-1)
  let of_int n = Int n

  let ceiling = ceiling_num
  let floor = floor_num
  let is_integer = is_integer_num
  let abs = abs_num

  let power a n =
    if n = 0 then one (* 0 ^ 0 = 1, undefined in mathematics*)
    else match a with
      | Int 1 -> one
      | Int 0 -> zero
      | Int (-1) ->
        if n mod 2 = 0 then one else m_one
      | _ -> power_num a (Int n)

  let modulo = mod_num
  let div = div_num
  let mult = mult_num
  let sub = sub_num
  let add = add_num
  let minus = minus_num
  let sign = sign_num
  let compare = compare_num
  let equal a b = a =/ b

  let string_of = string_of_num
  let of_string = num_of_string
  let float_of = float_of_num
  let z_of = big_int_of_num
  let of_z  = num_of_big_int

  let denominator = function 
    | Int _ | Big_int _ -> Big_int.unit_big_int
    | Ratio rat -> Ratio.denominator_ratio rat

  let numerator = function
    | Int i -> Big_int.big_int_of_int i
    | Big_int b -> b
    | Ratio rat -> Ratio.numerator_ratio rat

  let of_float x =
    if x = infinity || x = neg_infinity then raise Not_a_float;
    let (f, n) = frexp x in
    let z =
      Big_int.big_int_of_string
        (Int64.to_string (Int64.of_float (f *. 2. ** 52.))) in
    let factor = power (of_int 2) (n - 52) in
    mult (of_z z) factor


(********
         comparer avec l'implem de Alain de of_float
         let ratio_of_float f =
         Ratio.ratio_of_string (string_of_float f)
         
         let num_of_float f = num_of_ratio (ratio_of_float f)

         let of_float x = 
         let res = of_float x in
         let z = num_of_float x in
         assert (res =/ z);
         res
********)
end

(******************************************************************************)

module Z2 = struct

  include Z

  let my_gcd a b = 
    if sign a = 0 then b
    else if sign b = 0 then a
    else gcd a b

  let my_lcm a b = div (mul a b) (gcd a b)

end

module Q2 = struct

  exception Not_a_float
  include Q

  let m_one = neg one
  let minus = neg
  let mult = mul
  let numerator = num
  let denominator = den

  let of_z z = make z Z2.one
  let z_of = num

  let string_of = to_string

  let is_integer t = Z2.equal Z2.one (den t)

  let ceiling a = of_z (Z2.cdiv a.num a.den)

  let floor a = of_z (Z2.fdiv a.num a.den)

  let modulo t1 t2 = 
    assert (is_integer t1 && is_integer t2);
    {Q.num=Z2.rem t1.num t2.num; den = Z2.one}

  let power t n = make (Z2.pow t.num n) (Z2.pow t.den n)

  let float_of t = (Z2.to_float t.num) /. (Z2.to_float t.den)

end

(* XXX : 
   surprisingly, Z2 and Q2 (based on zarith) are not faster than 
   Z1 and Q1 (based on Nums)
*)
module Z = Z1
module Q = Q1
