let rec fib n =
    if n <= 2 then 1
    else fib (n-1) + fib (n-2);;

(*
let rec fibto n =
    match n with
    | 0 -> []
    | _ -> (fib n) :: fibto(n-1);;

let fibto n = 
    let rec aux buffer n =
        match n with
        | 0 -> List.rev buffer
        | _ -> aux ((fib n) :: buffer) (n-1)
    in aux [] n
*)
let fibto n = 
    let rec aux k = 
        let t = fib (k) in 
        if t >= n then []
        else t :: aux (k+1)
    in aux 1;;

let () = 
        if Array.length Sys.argv <> 2 then
            Printf.eprintf "Usage: %s n\n" Sys.argv.(0)
        else
            let n = int_of_string Sys.argv.(1) in
            let lista = fibto n in
            let rec imprime = function
            | [] -> print_newline ()
            | [x] -> print_int x
            | x :: resto -> print_int x; print_string ", "; imprime resto
        in
        imprime lista;
        print_newline ();;