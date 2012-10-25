{
open Parser;;
open Lexing;;

let debug=1;;

let incr_lineno lexbuf =
	let pos = lexbuf.lex_curr_p in
	lexbuf.lex_curr_p <- { pos with
		pos_lnum = pos.pos_lnum + 1;
		pos_bol = pos.pos_cnum;
	}
}

let digit      = ['0'-'9']
let whitespace = [' ' '\t' '\r']
let notspace   = [^ ' ' '\t' '\r' '\n']
let name       = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
	| '\n'                  { incr_lineno lexbuf; token lexbuf }			
	| digit+ as str         { INTEGER (int_of_string str) }					
	| '+'	                { PLUS }
	| '-'                   { MINUS }
	| '*'                   { TIMES }
	| '/'                   { DIVIDE }
	| "mod"                 { MOD }
	| '^'                   { POWER }
	| "and"                 { AND }
	| "or"                  { OR }
	| "not"                 { NOT }
	| "true"|"false" as str { BOOL(bool_of_string str) }
	| '='                   { EQUAL }
	| '<'                   { LESS }
	| '>'                   { GREATER }

	| "dup"                 { DUP }
	| "drop"                { DROP }
	| "dropall"             { DROPALL }
	| "swap"                { SWAP }
	| "over"                { OVER }
	| "rot"                 { ROT }

	| "read"                { READ }
	| "store"               { STORE }
	| ':'                   { COLON }
	| "jump"                { JUMP }
	| "ifjump"              { JUMP }
	| "sub"                 { SUB }
	| "esub"                { END_SUB }
	| name as str           { NAME (str) } 

	| whitespace            { token lexbuf }
	| "//"                  { sinlge_line_comment lexbuf }					
	| "/*"                  { multi_line_comment lexbuf }
	| _ as char 			{ raise (Failure("illegal character " ^  		
							Char.escaped char)) }							
	| eof                   { EOF }

and sinlge_line_comment = parse
	| '\n'          { Lexing.new_line lexbuf; token lexbuf }
	| _             { sinlge_line_comment lexbuf }

and multi_line_comment = parse
	| "*/"          { token lexbuf }
	| '\n'          { Lexing.new_line lexbuf; multi_line_comment lexbuf }
	| _             { multi_line_comment lexbuf }
