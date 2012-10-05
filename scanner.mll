{
open Ast

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

rule token = parse
	| digit+ as str { Int(int_of_string str) }
	| '+'	        { Plus }
	| '-'           { Minus }
	| '*'           { Times }
	| '/'           { Divide }
	| '\n'          { incr_linenum lexbuf; token lexbuf }
	| whitespace    { token lexbuf }
	| "//"       	{ sinlge_line_comment lexbuf }
	| "/*"       	{ multi_line_comment lexbuf }
	| eof           { raise End_of_file }

and sinlge_line_comment = parse
	| '\n'          { incr_linenum lexbuf; token lexbuf }
	| _             { sinlge_line_comment lexbuf }

and multi_line_comment = parse
	| "*/"          { token lexbuf }
	| '\n'          { incr_linenum lexbuf; multi_line_comment lexbuf }
	| _             { multi_line_comment lexbuf }
