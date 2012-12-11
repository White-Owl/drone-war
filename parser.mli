type token =
  | SUB
  | END_SUB
  | IF
  | ELSE
  | END_IF
  | BEGIN
  | WHILE
  | AGAIN
  | READ
  | STORE
  | COLON
  | JUMP
  | JUMPIF
  | LABEL of (string)
  | NAME of (string)
  | INTEGER of (int)
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | POWER
  | AND
  | OR
  | NOT
  | BOOL of (bool)
  | EQUAL
  | LESS
  | GREATER
  | DROP
  | DROPALL
  | DUP
  | SWAP
  | OVER
  | ROT
  | MOVE
  | STOP
  | SHOOT
  | LOOK
  | ISFOE
  | ISALLY
  | ISWALL
  | ISEND
  | WAIT
  | GETHEALTH
  | RANDOM
  | EOF

val drone :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.sub list
