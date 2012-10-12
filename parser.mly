%{ open Ast %}

%token <int> INTEGER
%token PLUS MINUS TIMES DIVIDE
%token AND OR NOT TRUE FALSE EQUAL LESS GREATER
%token DROP DROPALL DUP SWAP OVER ROT
%token <string> STORE READ
%token <string> SUB
%token END_SUB
%token <string> LABEL JUMP JUMP_IF
%token MOVE STOP SHOOT LOOK ISFOE ISALLY ISWALL ISEND WAIT GETHEALTH RANDOM
%token EOF

%start program


%%

program:
	  function | functions

functions:
	  functions function

function
	SUB NAME operations RETURN {}


	  INTEGER { Int($1) }