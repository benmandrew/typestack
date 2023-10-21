let read path =
  let open Stdio in
  let file = In_channel.create path in
  let lexbuf = Lexing.from_channel file in
  let ast = Parser.main Lexer.main lexbuf in
  In_channel.close file;
  ast
