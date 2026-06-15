let length_and_top n = 
  let rec aux cnt max n =
    match n with
    | 1 -> (cnt+1, max)
    | _ ->  if n > max then aux (cnt + 1) n (f n)
            else aux (cnt + 1) max (f n)
    in aux 0 0 n;;