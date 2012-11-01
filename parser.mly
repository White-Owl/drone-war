%{

open Ast;;
open Printf;;
open Lexing;;

%}

%token SUB END_SUB
%token READ STORE
%token COLON
%token JUMP JUMPIF
%token <string> LABEL
%token <string> NAME
%token <int> INTEGER
%token PLUS MINUS TIMES DIVIDE MOD POWER
%token AND OR NOT
%token <bool> BOOL
%token EQUAL LESS GREATER
%token DROP DROPALL DUP SWAP OVER ROT
%token MOVE STOP SHOOT LOOK ISFOE ISALLY ISWALL ISEND WAIT GETHEALTH RANDOM
%token EOF

/*
%start operations
%type <Ast.bytecode list> operations
*/

%start program
%type <Ast.program> program

%%

program:
	{ [], [] }                                      /* two lists for main body  of the program and for functions defined by users */
	| program operation { ($2 :: fst $1), snd $1 }  /* add operation to the first sub-list */
	| program sub { fst $1, ($2 :: snd $1) }        /* add user function to second sub-list */

sub:
	SUB NAME operations END_SUB  { { name = $2; body = List.rev $3; } } /* store the function name and function operations between "sub" and "esub" */


operations:
	{ [] }
	| operations operation   { if $2=Nop then $1 else $2 :: $1 }
	| operations error       { let start_pos = Parsing.rhs_start_pos 2 in
	                           (* let end_pos = Parsing.rhs_end_pos 2 in *)
	                           printf "Unrecognized tokens starting from line %d position %d\n" start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol +1);
	                           (* Here we are supposed to print the text which caused the error, but I have no idea how to reach lex_buffer from the parser.
	                           print_endline (String.sub Parser.lexbuf.lex_buffer start_pos.pos_cnum end_pos.pos_cnum); *)
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
	| NAME READ     { Read($1) }
	| NAME STORE    { Store($1) }
	| DROP          { Drop }
	| DROPALL       { Dropall }
	| DUP           { Dup }
	| SWAP          { Swap }
	| OVER          { Over }
	| ROT           { Rot }
	| LABEL         { Label($1) }
	| NAME JUMP     { Jump($1) }
	| NAME JUMPIF   { JumpIf($1) }
	| NAME          { Call($1) }
	| MOVE          { Move }
	| STOP          { Stop }
	| SHOOT         { Shoot }
	| LOOK          { Look }
	| ISFOE         { IsFoe }
	| ISALLY        { IsAlly }
	| ISWALL        { IsWall }
	| ISEND         { IsEnd }
	| WAIT          { Wait }
	| GETHEALTH     { GetHealth }
	| RANDOM        { Random }
