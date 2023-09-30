let rec aux e s =
  match (e, s) with
  | T.Val v, s -> v :: s
  | Add, T.N n0 :: N n1 :: s -> N (n0 + n1) :: s
  | Eql, N n0 :: N n1 :: s -> B (Int.equal n0 n1) :: s
  | Cond (e0, e1), B b :: s -> if b then aux e0 s else aux e1 s
  | Seq (e0, e1), s -> aux e0 s |> aux e1
  | _, _ -> failwith "Stuck configuration: bug in the typechecker?"

let v e = aux e []
