%{

open Ast;;
open Printf;;
open Lexing;;

%}

%token <int> INTEGER
%token <string> NAME
%token COLON
%token PLUS MINUS TIMES DIVIDE MOD POWER
%token AND OR NOT
%token <bool> BOOL
%token EQUAL LESS GREATER
%token DROP DROPALL DUP SWAP OVER ROT
%token <string> STORE READ
%token <string> SUB
%token END_SUB
%token <string> LABEL JUMP JUMP_IF
%token MOVE STOP SHOOT LOOK ISFOE ISALLY ISWALL ISEND WAIT GETHEALTH RANDOM
%token EOF

%start operations
%type <Ast.bytecode list> operations

%%

operations:
	   /* nothing */         { [] }
	| operations operation   { $2 :: $1}
	| operations error       { 	let start_pos = Parsing.rhs_start_pos 2 in
	                            (* let end_pos = Parsing.rhs_end_pos 2 in *)
	                            printf "Unrecognized tokens starting from line %d pos %d\n" start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol +1);
	                            (* Here we are supposed to print the text which caused the error, but I have no idea how to reach lex_buffer from the parser.
	                            print_endline (String.sub lexbuf.lex_buffer start_pos.pos_cnum end_pos.pos_cnum);  *)
	                            $1 }

operation:
	  INTEGER       { Int($1) }
	| PLUS          { Plus }
	| MINUS         { Minus }
	| TIMES         { Times }
	| DIVIDE        { Divide }
	| MOD           { Mod }
	| POWER         { Power }
	| AND           { And }
	| OR            { Or }
	| NOT           { Not }
	| BOOL          { Bool($1) }
	| EQUAL         { Equal }
	| LESS          { Less }
	| GREATER       { Greater }
	| LESS NAME     { Read($2) }
	| GREATER NAME  { Store($2) }
