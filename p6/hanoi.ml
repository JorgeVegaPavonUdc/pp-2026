
let rec hanoi n ori des =
  (* n número de discos, 1 <= ori <= 3, 1 <= dest <= 3, ori <> des *)
  if n = 0 then ""
  else
    let otro = otro ori des in
    hanoi (n-1) ori otro ^ mueve (ori, des) ^ hanoi (n-1) otro des

let hanoi n ori des =
  if n = 0 || ori = des then ""
  else hanoi n ori des

let print_hanoi n ori des =
  if n < 0 || ori < 1 || ori > 3 || des < 1 || des > 3
  then print_endline "**ERROR**"
  else print_endline ("=========\n" ^
                      hanoi n ori des ^
                      "=========")

let crono f x =
  let t = Sys.time () in
  f x; Sys.time () -. t

