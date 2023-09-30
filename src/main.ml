open Core

type v = N of int | B of bool [@@deriving compare, sexp_of, show]
type e = Val of v | Add | Eql | Cond of e * e | Seq of e * e
type stack = v list [@@deriving compare, sexp_of, show]
type t = Num | Bool [@@deriving compare]
type stack_t = t list

module Type_check = struct
  let t_equal t0 t1 = Int.equal (compare t0 t1) 0

  let rec infer e s =
    match (e, s) with
    | Val (N _), Some s -> Some (Num :: s)
    | Val (B _), Some s -> Some (Bool :: s)
    | Add, Some (Num :: Num :: s) -> Some (Num :: s)
    | Eql, Some (Num :: Num :: s) -> Some (Bool :: s)
    | Cond (e0, e1), Some (Bool :: s) -> (
        match (infer e0 (Some s), infer e1 (Some s)) with
        | Some s0, Some s1 -> if List.equal t_equal s0 s1 then Some s0 else None
        | _, _ -> None)
    | Seq (e0, e1), s -> infer e0 s |> infer e1
    | _, _ -> None

  let v e s = infer e (Some s) |> Option.is_some
end

module Exec = struct
  let rec aux e s =
    match (e, s) with
    | Val v, s -> v :: s
    | Add, N n0 :: N n1 :: s -> N (n0 + n1) :: s
    | Eql, N n0 :: N n1 :: s -> B (Int.equal n0 n1) :: s
    | Cond (e0, e1), B b :: s -> if b then aux e0 s else aux e1 s
    | Seq (e0, e1), s -> aux e0 s |> aux e1
    | _, _ -> failwith "Stuck configuration: bug in the typechecker?"

  let v e = aux e []
end

let () =
  let e = Seq (Val (N 0), Seq (Val (N 1), Add)) in
  let out =
    if Type_check.v e [] then Exec.v e |> show_stack
    else "Expression does not typecheck"
  in
  Printf.printf "%s\n" out
