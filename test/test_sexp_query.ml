open! Core
open! Async
open! Import

let test input program =
  let program = Sexp.to_string_mach [%sexp (program : string Sexp_query.Generic.t)] in
  let stdin = String.concat ~sep:"\n" (List.map input ~f:Sexp.to_string_mach) in
  run "../bin/main.exe" [ "sexp-query"; program ] ~stdin
;;

let input = [ [%sexp "foo"]; [%sexp "bar"]; [%sexp "foo", "bar"] ]

let%expect_test "this" =
  let%bind () = test input This in
  [%expect {|
    foo
    bar
    (foo bar) |}]
;;

let%expect_test "regex" =
  let%bind () = test input (Regex "o+") in
  [%expect {| foo |}]
;;

let%expect_test "quote" =
  let%bind () = test input (Quote [%sexp "_"]) in
  [%expect {|
    _
    _
    _ |}]
;;

let%expect_test "or" =
  let%bind () = test input (Or [ Regex "o+"; Quote [%sexp "rejected"] ]) in
  [%expect {|
    foo
    rejected
    rejected |}]
;;

let%expect_test "pipe" =
  let%bind () =
    Pipe [ Regex "o+"; Or [ Regex "bar"; Quote [%sexp "_"] ] ]
    |> test [ [%sexp "foo"]; [%sexp "foobar"]; [%sexp "baz"] ]
  in
  [%expect {|
    _
    foobar |}]
;;

let%expect_test "cat" =
  let%bind () = test input (Cat [ This; Quote [%sexp "that"] ]) in
  [%expect {|
    foo
    that
    bar
    that
    (foo bar)
    that |}]
;;

let%expect_test "concat" =
  let%bind () = test input Concat in
  [%expect {|
    foo
    bar
    foobar |}]
;;

let%expect_test "wrap" =
  let%bind () = test input (Wrap (Cat [ This; Quote [%sexp "that"] ])) in
  [%expect {|
    (foo that)
    (bar that)
    ((foo bar) that) |}]
;;

let%expect_test "not" =
  let%bind () = test input (Not (Regex "o+")) in
  [%expect {|
    bar
    (foo bar) |}]
;;

let%expect_test "test" =
  let%bind () = test input (Test (Pipe [ Regex "o+"; Quote [%sexp "_"] ])) in
  [%expect {|
    foo |}]
;;

let%expect_test "equals" =
  let%bind () = test input (Equals [%sexp "foo", "bar"]) in
  [%expect {| (foo bar) |}]
;;

let%expect_test "crackle-pop" =
  let divisible_by_three_pattern =
    (* https://www.quaxio.com/triple/ *)
    "^([0369]|[258][0369]*[147]|[147]([0369]|[147][0369]*[258])*[258]|[258][0369]*[258]([0369]|[147][0369]*[258])*[258]|[147]([0369]|[147][0369]*[258])*[147][0369]*[147]|[258][0369]*[258]([0369]|[147][0369]*[258])*[147][0369]*[147])*$"
  in
  let%bind () =
    test
      (List.init 15 ~f:(fun i -> [%sexp (i + 1 : int)]))
      (Or
         [ Pipe
             [ Wrap
                 (Cat
                    [ Pipe [ Regex divisible_by_three_pattern; Quote [%sexp "Crackle"] ]
                    ; Pipe [ Regex "[05]$"; Quote [%sexp "Pop"] ]
                    ])
             ; Not (Equals [%sexp []])
             ; Concat
             ]
         ; This
         ])
  in
  [%expect
    {|
    1
    2
    Crackle
    4
    Pop
    Crackle
    7
    8
    Crackle
    Pop
    11
    Crackle
    13
    14
    CracklePop |}]
;;
