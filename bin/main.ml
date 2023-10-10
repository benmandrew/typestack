open Typestack

let () =
  let e = T.Seq (Val (N 0), Seq (Val (N 1), Add)) in
  let out =
    if Typecheck.v e [] then Exec.v e |> T.show_v_stack
    else "Expression does not typecheck"
  in
  Printf.printf "%s\n" out
