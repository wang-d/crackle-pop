open! Core
open! Async

let is_divisible i ~by = 0 = i mod by

let crackle_pop i =
  List.filter_opt
    [ Option.some_if (is_divisible i ~by:3) "Crackle"
    ; Option.some_if (is_divisible i ~by:5) "Pop"
    ]
  |> String.concat
  |> Option.return
  |> Option.filter ~f:(Fn.non String.is_empty)
  |> Option.value ~default:(Int.to_string i)
;;

let () =
  Command.async
    ~summary:"Solve RC's Crackle Pop problem"
    (let%map_open.Command.Let_syntax () = return () in
     fun () ->
       List.init 100 ~f:(Int.( + ) 1)
       |> List.map ~f:crackle_pop
       (* I assume they want me to print newlines too? *)
       |> List.iter ~f:print_endline;
       return ())
  |> Command.run
;;
