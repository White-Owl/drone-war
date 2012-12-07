{
open Parser_dbt;;
open String;;
open Lexing;;

let create_hashtable size init =
	let tbl = Hashtbl.create size in
	List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
	tbl

let keyword_table =
	create_hashtable 8 [
		("if",          IF);
		("then",        THEN);
		("else",        ELSE);
		("do",          DO);
		("loop",        LOOP);
		("while",       WHILE);
		("until",       UNTIL);
		("exit",        EXIT);
		("sub",         SUB);
		("function",    FUNCTION);
		("call",        CALL);
		("end",         END);
		("for",         FOR);
		("to",          TO);
		("step",        STEP);
		("next",        NEXT);
		("goto",        GOTO);
		("true",        BOOL(true));
		("false",       BOOL(false));
		("and",         AND);
		("or",          OR);
		("not",         NOT);

		("sleep",       SLEEP);
		("move",        MOVE);
		("stop",        STOP);
		("shoot",       SHOOT);
		("rnd",         RANDOM);
		("health",      HEALTH);

		("startscan",   STARTSCAN);
		("nextscan",    NEXTSCAN);
		("cancelscan",  CANCELSCAN);
		(".isend",      ISEND);
		(".iswall",     ISWALL);
		(".isfoe",      ISFOE);
		(".isally",     ISALLY);
		(".distance",   DISTANCE);
		(".direction",  DIRECTION);
	]

exception Unknown_token of string * int * int;;

let incr_lineno lexbuf =
	let pos = lexbuf.lex_curr_p in
	lexbuf.lex_curr_p <- { pos with
		pos_lnum = pos.pos_lnum + 1;
		pos_bol = pos.pos_cnum;
	}
}

let digit = ['0' - '9']
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* | '.' ['a'-'z' 'A'-'Z']+
let space = [' ' '\t' '\r']
let not_space = [^ ' ' '\t' '\r']

rule drone_basic = parse
  | digit+ as inum  { let num = int_of_string inum in INT num }
  | id as word  { try
                    let token = Hashtbl.find keyword_table (String.lowercase word) in
                    token
                  with Not_found -> ID (String.lowercase word)
                }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | ':'      { COLON }
  | ','      { COMMA }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { TIMES }
  | '/'      { DIVIDE }
  | '='      { EQUAL }
  | "<>"     { NOT_EQUAL }
  | '<'      { LESS }
  | "<="     { LESS_EQUAL }
  | ">"      { GREATER }
  | ">="     { GREATER_EQUAL }

  | '\'' [^ '\n']* 	(* eat up one-line comments *)
  | space           (* eat up whitespace *)
                    { drone_basic lexbuf }

  | '\n'            { incr_lineno lexbuf; CR }

(*  | not_space * as str { raise (Unknown_token (str, lexbuf.lex_curr_p.pos_lnum, lexbuf.lex_start_p.pos_cnum-lexbuf.lex_start_p.pos_bol +1)  ) } *)

  | eof             { EOF }
