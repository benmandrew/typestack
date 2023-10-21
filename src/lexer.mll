{
open Tokens

type error = Illegal_character of char

exception Error of error
}

rule main = parse
  | [' ' '\t' '\n']
      { main lexbuf }
  | ['0'-'9']+ as i
      { INT (int_of_string i) }
  | "true"
      { BOOL true }
  | "false"
      { BOOL false }
  | "Add"
      { ADD }
  | "Eql"
      { EQL }
  | "Cond"
      { COND }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | ","
      { COMMA }
  | ";"
      { SEMICOLON }
  | eof
      { EOF }
  | (_ as illegal_char)
    { raise
        (Error
          (Illegal_character illegal_char)) }
