type v = N of int | B of bool
type e = Val of v | Add | Eql | Cond of e * e | Skip | Seq of e * e
type stack = v list
type t = Num | Bool
type stack_t = t list
