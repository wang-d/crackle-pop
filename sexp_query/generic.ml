open! Core

type 're t =
  | Cat of 're t list
  | Concat
  | Equals of Sexp.t
  | Not of 're t
  | Or of 're t list
  | Pipe of 're t list
  | Quote of Sexp.t
  | Regex of 're
  | Test of 're t
  | This
  | Wrap of 're t
[@@deriving sexp]
