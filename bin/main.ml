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

let simple =
  Command.async
    ~summary:"a simple CracklePop solution"
    (let%map_open.Command.Let_syntax () = return () in
     fun () ->
       List.init 100 ~f:(Int.( + ) 1)
       |> List.map ~f:crackle_pop
       (* I assume they want me to print newlines too? *)
       |> List.iter ~f:print_endline;
       return ())
;;

let sexp_query =
  Command.async
    ~summary:""
    (let%map_open.Command.Let_syntax () = return ()
     and program =
       anon ("PROGRAM" %: sexp_conv [%of_sexp: string Sexp_query.Generic.t])
     in
     fun () ->
       let%bind input = Reader.contents (force Reader.stdin) in
       Parsexp.Many.parse_string_exn input
       |> Sequence.of_list
       |> Sexp_query.Eval.f program
       |> Sequence.iter ~f:print_s;
       return ())
;;

let () =
  Command.group
    ~summary:"CracklePop solutions"
    [ "sexp-query", sexp_query; "simple", simple ]
  |> Command.run
;;
