{
open Ast

exception Unrecognized_Token of string;;

let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
  ;;
}

let digit      = ['0'-'9']
let whitespace = [' ' '\t' '\r']
let notspace   = [^ ' ' '\t' '\r' '\n']
let name       = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
	| '\n'                  { incr_linenum lexbuf; token lexbuf }
	| digit+ as str         { Int(int_of_string str) }
	| '+'	                { Plus }
	| '-'                   { Minus }
	| '*'                   { Times }
	| '/'                   { Divide }
	| "mod"                 { Mod }
	| '^'                   { Power }
	| "and"                 { And }
	| "or"                  { Or }
	| "not"                 { Not}
	| "true"|"false" as str { Bool(bool_of_string str) }
	| '='                   { Equal }
	| '<'                   { prefix Less lexbuf }
	| '>'                   { prefix Greater lexbuf }
	| ':'                   { prefix Colon lexbuf }

	| whitespace            { token lexbuf }
	| "//"                  { sinlge_line_comment lexbuf }
	| "/*"                  { multi_line_comment lexbuf }
	| notspace + as str     { raise (Unrecognized_Token str) }
	| eof                   { raise End_of_file }

and sinlge_line_comment = parse
	| '\n'          { incr_linenum lexbuf; token lexbuf }
	| _             { sinlge_line_comment lexbuf }

and multi_line_comment = parse
	| "*/"          { token lexbuf }
	| '\n'          { incr_linenum lexbuf; multi_line_comment lexbuf }
	| _             { multi_line_comment lexbuf }

and prefix symbol = parse
	| whitespace            { symbol }
	| '\n'                  { incr_linenum lexbuf; symbol }
	| name as str           { match symbol with
							  | Greater -> Store(str)
							  | Less    -> Read(str)
							  | Colon   -> Function(str)
							  | _ -> raise (Unrecognized_Token "bad prefix")
							}
	| notspace + as str     { raise (Unrecognized_Token str) }
