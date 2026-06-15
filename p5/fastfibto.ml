let fastfibto n =
  let rec aux a b =
    if a > n then [b]
    else b :: aux (a+b) a
  in
  aux 1 1;;