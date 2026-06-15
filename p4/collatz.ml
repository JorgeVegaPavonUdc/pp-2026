let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1;;

let rec check n =
  n = 1 || check (f n);;

let rec check_to = function
  | 1 -> true
  | n -> 
      check(n) && check_to(n-1);;

let orbit n =  
  let rec aux n =
    match n with
    | 1 -> "1"
    | _ -> string_of_int (n) ^ ", " ^ aux (f(n))
  in aux n;;

let top n = 
  if  n < 1 then 1
  else  
  let rec aux acc n = 
    match n with
    | 1 -> acc
    | _ ->  if n > acc then aux n (f n)
            else aux acc (f n)
    in aux 0 n;;
