type token =
  | CR
  | IF
  | THEN
  | ELSE
  | DO
  | LOOP
  | WHILE
  | UNTIL
  | EXIT
  | SUB
  | FUNCTION
  | CALL
  | END
  | FOR
  | TO
  | STEP
  | NEXT
  | GOTO
  | BOOL of (bool)
  | ID of (string)
  | INT of (int)
  | LPAREN
  | RPAREN
  | COLON
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQUAL
  | NOT_EQUAL
  | LESS
  | GREATER
  | LESS_EQUAL
  | GREATER_EQUAL
  | AND
  | OR
  | NOT
  | SLEEP
  | MOVE
  | STOP
  | SHOOT
  | RANDOM
  | HEALTH
  | STARTSCAN
  | NEXTSCAN
  | CANCELSCAN
  | ISEND
  | ISWALL
  | ISFOE
  | ISALLY
  | DISTANCE
  | DIRECTION
  | EOF

val drone :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.sub list
