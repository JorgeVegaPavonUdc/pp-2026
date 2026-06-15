let e_aprox n = 
  let rec aux sum fact i = 
    if i > n then sum
    else 
      let new_fact = float_of_int (i + 1) *. fact in
      let new_sum = sum +. (1. /. fact) in
      aux new_sum new_fact (i+1)
    in aux 0.0 1.0 0


let() =
    let n = 14 in 
    let e = e_aprox n in 
    print_endline ("El valor de e con " ^ string_of_int n ^ " aproximaciones es: " ^ string_of_float e)