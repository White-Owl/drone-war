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
;;
exception Unknown_token of string * int * int;;

}

let digit      = ['0'-'9']
let space      = [' ' '\t']
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
	| name ':' as str       { LABEL (String.sub str 0 ((String.length str)-1) ) }
	| "jump"                { JUMP }
	| "jumpif"              { JUMPIF }
	| "sub"                 { SUB }
	| "end" space+ "sub"    { END_SUB }
	| "if"                  { IF }
	| "else"                { ELSE }
	| "end" space+ "if"     { END_IF }

	| "move"                { MOVE }
	| "stop"                { STOP }
	| "shoot"               { SHOOT }
	| "look"                { LOOK }
	| "wait"                { WAIT }
	| "gethealth"           { GETHEALTH }
	| "random"              { RANDOM }
	| "isfoe"               { ISFOE }
	| "isally"              { ISALLY }
	| "iswall"              { ISWALL }
	| "isend"               { ISEND }

	| name as str           { NAME (str) }

	| whitespace            { token lexbuf }
	| "//"                  { sinlge_line_comment lexbuf }
	| "/*"                  { multi_line_comment lexbuf }
	| notspace * as str     { raise (Unknown_token (str, lexbuf.lex_curr_p.pos_lnum, lexbuf.lex_start_p.pos_cnum-lexbuf.lex_start_p.pos_bol +1)  ) }
	| eof                   { EOF }

and sinlge_line_comment = parse
	| '\n'          { Lexing.new_line lexbuf; token lexbuf }
	| _             { sinlge_line_comment lexbuf }

and multi_line_comment = parse
	| "*/"          { token lexbuf }
	| '\n'          { Lexing.new_line lexbuf; multi_line_comment lexbuf }
	| _             { multi_line_comment lexbuf }
