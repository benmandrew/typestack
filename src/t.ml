open Core

type v = N of int | B of bool [@@deriving compare, sexp_of, show]
type e = Val of v | Add | Eql | Cond of e * e | Seq of e * e
type stack = v list [@@deriving compare, sexp_of, show]
type t = Num | Bool [@@deriving compare]
type stack_t = t list
