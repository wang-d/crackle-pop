open! Core

type t = Re.re Generic.t

let rec compile : string Generic.t -> t = function
  | Cat ts -> Cat (List.map ts ~f:compile)
  | Concat -> Concat
  | Equals sexp -> Equals sexp
  | Not t -> Not (compile t)
  | Or ts -> Or (List.map ts ~f:compile)
  | Pipe ts -> Pipe (List.map ts ~f:compile)
  | Quote sexp -> Quote sexp
  | Regex pattern -> Regex (Re.Posix.compile_pat pattern)
  | Test t -> Test (compile t)
  | This -> This
  | Wrap t -> Wrap (compile t)
;;

let eval_concat sexps =
  let rec atoms : Sexp.t -> string list = function
    | Atom a -> [ a ]
    | List sexps -> List.bind sexps ~f:atoms
  in
  Sequence.map sexps ~f:(fun sexp -> Sexp.Atom (String.concat (atoms sexp)))
;;

let rec eval (t : t) (sexps : Sexp.t Sequence.t) =
  Sequence.memoize
    (match t with
    | Cat ts -> Sequence.bind (Sequence.of_list ts) ~f:(fun t -> eval t sexps)
    | Concat -> eval_concat sexps
    | Equals sexp -> Sequence.filter sexps ~f:(Sexp.( = ) sexp)
    | Not t ->
      Sequence.bind sexps ~f:(fun sexp ->
          let sexp = Sequence.singleton sexp in
          let output = eval t sexp in
          match Sequence.is_empty output with
          | true -> sexp
          | false -> Sequence.empty)
    | Or ts -> eval_or ts sexps
    | Pipe ts -> eval_pipe ts sexps
    | Quote sexp -> Sequence.map sexps ~f:(const sexp)
    | Regex re ->
      Sequence.filter sexps ~f:(function
          | List _ -> false
          | Atom a -> Re.execp re a)
    | Test t ->
      Sequence.filter sexps ~f:(fun sexp ->
          not (Sequence.is_empty (eval t (Sequence.singleton sexp))))
    | This -> sexps
    | Wrap t -> Sequence.singleton (Sexp.List (Sequence.to_list (eval t sexps))))

and eval_or ts sexps =
  List.find_map ts ~f:(fun t ->
      let output = eval t sexps in
      match Sequence.is_empty output with
      | true -> None
      | false -> Some output)
  |> Option.value ~default:Sequence.empty

and eval_pipe ts sexps =
  match ts with
  | [] -> sexps
  | t :: ts ->
    let sexps = eval t sexps in
    (match Sequence.is_empty sexps with
    | true -> sexps
    | false -> eval_pipe ts sexps)
;;

let f program sexps =
  let t = compile program in
  Sequence.bind sexps ~f:(fun sexp -> eval t (Sequence.singleton sexp))
;;
