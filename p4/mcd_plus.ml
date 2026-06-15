let mcd_pasos (x, y) =
  let rec aux cnt (x,y) = 
      match y with
      | 0 -> (cnt+1, x)
      | _ -> aux (cnt +1) (y,(x mod y))
  in aux 0 (x,y);;