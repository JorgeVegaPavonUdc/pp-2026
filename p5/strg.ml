let str3_of_int n = 
  let rec aux n =
    match n with 
    | 0 -> ""
    | _ -> aux (n/3) ^ string_of_int (n mod 3) 
  in aux n;;

let int_of_str3 s =
  let n = String.length s in
  let rec aux i acc =
    if i = n then acc
    else
      let d = Char.code s.[i] - Char.code '0' in
      aux (i+1) (acc * 3 + d)
  in
  aux 0 0;;