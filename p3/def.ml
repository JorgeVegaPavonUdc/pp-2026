(*1/1 - 1/3 + 1/5 - 1/7*)
(*0*2 +1, 1*2+1, 2*2+1, 3*2 +1*)
let pi = 
  let rec aux sum i n = 
    if i > n then sum *. 4.
    else 
      let term = 1. /. float_of_int(i*2 + 1) in 
      let sign = if i mod 2 = 0 then 1. else -1. in 
      let new_sum = sum +. term *. sign in
      aux new_sum (i+1) n
    in aux 0.0 0 30


(*1/0! + 1/1! + 1/2! + 1/3! + 1/4!*)
(*i, i, i, i*)
let e = 
  let rec aux sum fact i n = 
    if i > n then sum 
    else
    let new_fact = fact *. float_of_int (i+1) in
    let new_sum = sum +. 1. /. fact in 
    aux new_sum new_fact (i+1) n
  in aux 0.0 1.0 0 30

let max_int_f = 
  let rec aux n = 
    let next = n *. 2. +. 1. in 
    if next = infinity then n
    else aux next 
  in aux 1.

let perimeter = function 
  |x when x <= 0. -> 0.
  |x -> x *. 2. *. pi  

let area = function
  |x when x <= 0. -> 0.
  |x -> x *. x*. pi 

let next_char = function
  |c -> Char.chr(Char.code(c) + 1)
let abs_f = function
  | x when x <= 0. -> x *. -1.
  | x -> x

let odd = 
  function x -> x mod 2 = 1


(*10 mod 5 = 0 -> 10 + 0*)
(*12 mod 5 = 2 -> 12 + 3*)

let next_5_mult = function 
  |x when (x mod 5 = 0) -> x
  |x -> x + (5 - (x mod 5))

let is_letter = function
    | c when 'a' <= c && c <= 'z' -> true
    | c when 'A' <= c && c <= 'Z' -> true
    | _ -> false

let string_of_bool = function
  |true -> "True"
  |_ -> "False" 



let () = 
  (* Pi y E *)
  print_endline ("Pi          = " ^ string_of_float pi);
  print_endline ("E           = " ^ string_of_float e);
  print_endline ("Max_int     = " ^ string_of_float max_int_f);

  (* Perimeter y area - hay un bug: area debería ser x *. x *. pi, no x *. pi**2 *)
  print_endline ("Perimeter 5 = " ^ string_of_float (perimeter 5.));
  print_endline ("Perimeter -1= " ^ string_of_float (perimeter (-1.)));
  print_endline ("Area 5      = " ^ string_of_float (area 5.));
  print_endline ("Area -1     = " ^ string_of_float (area (-1.)));

  (* Next char *)
  print_endline ("Next 'a'    = " ^ String.make 1 (next_char 'a'));
  print_endline ("Next 'z'    = " ^ String.make 1 (next_char 'z'));

  (* Abs *)
  print_endline ("Abs -5.     = " ^ string_of_float (abs_f (-5.)));
  print_endline ("Abs 5.      = " ^ string_of_float (abs_f 5.));

  (* Odd (en realidad es even, devuelve true si es par) *)
  print_endline ("Odd 4       = " ^ string_of_bool (odd 4));
  print_endline ("Odd 3       = " ^ string_of_bool (odd 3));

  (* Next 5 mult *)
  print_endline ("Next5 7     = " ^ string_of_int (next_5_mult 7));
  print_endline ("Next5 10    = " ^ string_of_int (next_5_mult 10));

  (* Is letter *)
  print_endline ("IsLetter 'a'= " ^ string_of_bool (is_letter 'a'));
  print_endline ("IsLetter '3'= " ^ string_of_bool (is_letter '3'))