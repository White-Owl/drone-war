{
open Parser;;
open Lexing;;

exception Unrecognized_Token of string;;
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
	| ':'                   { COLON }
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

	| whitespace            { token lexbuf }
	| "//"                  { sinlge_line_comment lexbuf }
	| "/*"                  { multi_line_comment lexbuf }
	| name as str           { NAME (str) }
(*	| notspace + as str     { raise (Unrecognized_Token str) } *)
	| eof                   { EOF }

and sinlge_line_comment = parse
	| '\n'          { Lexing.new_line lexbuf; token lexbuf }
	| _             { sinlge_line_comment lexbuf }

and multi_line_comment = parse
	| "*/"          { token lexbuf }
	| '\n'          { Lexing.new_line lexbuf; multi_line_comment lexbuf }
	| _             { multi_line_comment lexbuf }
