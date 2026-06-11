
let is_prime n =
  let rec check_from i =
    i >= n || (n mod i <> 0 && check_from (i+1))
  in n > 1 && check_from 2

