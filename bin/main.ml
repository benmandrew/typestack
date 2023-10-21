open Typestack

let run file =
  let e = Io.read file in
  let out =
    if Typecheck.v e [] then Exec.v e |> T.show_v_stack
    else "Expression does not typecheck"
  in
  Printf.printf "%s\n" out

(* Command-line parsing *)
open Cmdliner

let source_file =
  Arg.required
  @@ Arg.pos 0 Arg.(some file) None
  @@ Arg.info ~doc:"The input typestack source file" ~docv:"SOURCE" []

let () =
  let doc = "Type-check and run a typestack program" in
  let info = Cmd.info "typestack" ~doc in
  let cmd_t = Term.(const run $ source_file) in
  exit @@ Cmd.eval @@ Cmd.v info cmd_t
