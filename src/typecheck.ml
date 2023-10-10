let t_stack_eq t0 t1 = Int.equal (T.compare_t_stack t0 t1) 0

let rec infer e s =
  match (e, s) with
  | T.Val (N _), Some s -> Some (T.Num :: s)
  | Val (B _), Some s -> Some (Bool :: s)
  | Add, Some (T.Num :: Num :: s) -> Some (Num :: s)
  | Eql, Some (Num :: Num :: s) -> Some (Bool :: s)
  | Cond (e0, e1), Some (Bool :: s) -> (
      match (infer e0 (Some s), infer e1 (Some s)) with
      | Some s0, Some s1 -> if t_stack_eq s0 s1 then Some s0 else None
      | _, _ -> None)
  | Seq (e0, e1), s -> infer e0 s |> infer e1
  | _, _ -> None

let v e s = infer e (Some s) |> Option.is_some
