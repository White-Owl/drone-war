%{

open Ast;;
open Printf;;
open Lexing;;
open Utils;;

let auto_label_counter = ref 0;;

let make_label() =
	incr auto_label_counter;
	("-" ^ string_of_int(!auto_label_counter))
	;;

%}

%token SUB END_SUB
%token IF ELSE END_IF
%token BEGIN WHILE AGAIN
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
%token MOVE STOP SHOOT LOOK ISFOE ISALLY ISWALL WAIT GETHEALTH RANDOM
%token EOF

%start drone
%type <Ast.sub list> drone

%%

drone:
  program { let main_sub = { name="--"; body = List.rev (fst $1); } in
			main_sub :: snd $1 }

program:
	{ [], [] }                                      /* two lists for main body  of the program and for functions defined by users */
	| program operation { ($2 :: fst $1), snd $1 }  /* add operations to the body of the main program */
	| program sub { fst $1, ($2 :: snd $1) }        /* add user function to the list of subs */
	| program compaund_statment { ($2 @ fst $1), snd $1 }

sub:
	SUB NAME operations END_SUB  	{ { name = $2; body = List.rev $3; } } /* store the function name and function operations between "sub" and "esub" */


operations:
	{ [] }
	| operations operation   { if $2=Nop then $1 else $2 :: $1 }
	| operations compaund_statment    { $2 @ $1 }
	| operations error       { let pos = Parsing.rhs_start_pos 2 in
	                           raise (Parse_failure ("Unrecognized tokens starting from line %d position %d\n", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol +1)));
	                         }


compaund_statment:
	IF operations END_IF     {  let lbl = make_label() in
								( Label(lbl):: $2 ) @ [ JumpIf(lbl) ; Not ]
							 }
	| IF operations ELSE operations END_IF	{ let lbl1 = make_label() and lbl2= make_label() in
								 ( Label(lbl2):: $4) @ ( Label(lbl1)::(Jump(lbl2):: $2 )) @ [ JumpIf(lbl1) ; Not ]
							 }
	| BEGIN operations AGAIN { let lbl=make_label() in
								(Jump(lbl)::$2) @ [Label(lbl)]
							}
	| BEGIN operations WHILE operations AGAIN	{let lbl1 =make_label() and lbl2 = make_label() in
								[Label(lbl1); Jump(lbl2)] @ $4 @ [ JumpIf(lbl1) ; Not ] @ $2 @ [Label(lbl2)]
							}

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
	| WAIT          { Wait }
	| GETHEALTH     { GetHealth }
	| RANDOM        { Random }
