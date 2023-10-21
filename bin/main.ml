open Typestack

let () =
  let e = Io.read "example.tsk" in
  let out =
    if Typecheck.v e [] then Exec.v e |> T.show_v_stack
    else "Expression does not typecheck"
  in
  Printf.printf "%s\n" out
