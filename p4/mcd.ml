let rec mcd (x, y) =
  match (x, y) with
  | (x, 0) -> x
  | (0, y) -> y
  | (x, y) when x > y -> mcd (x - y, y)
  | (x, y)            -> mcd (x, y - x)

let rec mcd' (x,y) = 
  match y with
  | 0 -> x
  | _ -> mcd' (y,(x mod y));;