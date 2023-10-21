%start main
%type <T.e> main
%type <T.e> expr

%%

main:
  | e = expr EOF
      { e }

expr:
  | i = INT
      { Val (N (i)) }
  | b = BOOL
      { Val (B (b)) }
  | ADD
      { Add }
  | EQL
      { Eql }
  | COND LPAREN e0 = expr COMMA e1 = expr RPAREN
      { Cond (e0, e1) }
  | e0 = expr SEMICOLON e1 = expr
      { Seq (e0, e1) }
  | LPAREN e = expr RPAREN
      { ( e ) }
;
