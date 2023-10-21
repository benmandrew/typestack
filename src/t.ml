open Core

(* Values *)
type v = N of int | B of bool [@@deriving compare, sexp_of, show]
type v_stack = v list [@@deriving compare, sexp_of, show]

(* Expressions *)
type e = Val of v | Add | Eql | Cond of e * e | Seq of e * e
[@@deriving compare, sexp_of]

(* Types *)
type t = Num | Bool [@@deriving compare]
type t_stack = t list [@@deriving compare]
