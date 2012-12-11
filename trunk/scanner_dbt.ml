# 1 "scanner_dbt.mll"
 
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

# 65 "scanner_dbt.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\237\255\238\255\239\255\001\000\002\000\030\000\245\255\
    \246\255\247\255\248\255\249\255\250\255\251\255\252\255\253\255\
    \058\000\133\000\210\000\203\000\242\255\244\255\240\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\016\000\014\000\012\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\001\000\000\000\001\000\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\255\255\000\000\000\000\000\000\004\000\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \255\255\255\255\255\255\255\255\000\000\000\000\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\003\000\002\000\255\255\000\000\003\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
    \015\000\014\000\009\000\011\000\012\000\010\000\016\000\008\000\
    \018\000\018\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\013\000\000\000\006\000\007\000\005\000\022\000\
    \000\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\020\000\021\000\000\000\000\000\000\000\
    \000\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \001\000\255\255\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\004\000\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\000\000\000\000\005\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\006\000\006\000\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \000\000\004\000\018\000\018\000\018\000\018\000\018\000\018\000\
    \018\000\018\000\018\000\018\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\019\000\019\000\
    \019\000\019\000\019\000\019\000\019\000\019\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec drone_basic lexbuf =
    __ocaml_lex_drone_basic_rec lexbuf 0
and __ocaml_lex_drone_basic_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
let
# 70 "scanner_dbt.mll"
              inum
# 221 "scanner_dbt.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 70 "scanner_dbt.mll"
                    ( let num = int_of_string inum in INT num )
# 225 "scanner_dbt.ml"

  | 1 ->
let
# 71 "scanner_dbt.mll"
          word
# 231 "scanner_dbt.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 71 "scanner_dbt.mll"
                ( try
                    let token = Hashtbl.find keyword_table (String.lowercase word) in
                    token
                  with Not_found -> ID (String.lowercase word)
                )
# 239 "scanner_dbt.ml"

  | 2 ->
# 76 "scanner_dbt.mll"
             ( LPAREN )
# 244 "scanner_dbt.ml"

  | 3 ->
# 77 "scanner_dbt.mll"
             ( RPAREN )
# 249 "scanner_dbt.ml"

  | 4 ->
# 78 "scanner_dbt.mll"
             ( COLON )
# 254 "scanner_dbt.ml"

  | 5 ->
# 79 "scanner_dbt.mll"
             ( COMMA )
# 259 "scanner_dbt.ml"

  | 6 ->
# 80 "scanner_dbt.mll"
             ( PLUS )
# 264 "scanner_dbt.ml"

  | 7 ->
# 81 "scanner_dbt.mll"
             ( MINUS )
# 269 "scanner_dbt.ml"

  | 8 ->
# 82 "scanner_dbt.mll"
             ( TIMES )
# 274 "scanner_dbt.ml"

  | 9 ->
# 83 "scanner_dbt.mll"
             ( DIVIDE )
# 279 "scanner_dbt.ml"

  | 10 ->
# 84 "scanner_dbt.mll"
             ( EQUAL )
# 284 "scanner_dbt.ml"

  | 11 ->
# 85 "scanner_dbt.mll"
             ( NOT_EQUAL )
# 289 "scanner_dbt.ml"

  | 12 ->
# 86 "scanner_dbt.mll"
             ( LESS )
# 294 "scanner_dbt.ml"

  | 13 ->
# 87 "scanner_dbt.mll"
             ( LESS_EQUAL )
# 299 "scanner_dbt.ml"

  | 14 ->
# 88 "scanner_dbt.mll"
             ( GREATER )
# 304 "scanner_dbt.ml"

  | 15 ->
# 89 "scanner_dbt.mll"
             ( GREATER_EQUAL )
# 309 "scanner_dbt.ml"

  | 16 ->
# 93 "scanner_dbt.mll"
                    ( drone_basic lexbuf )
# 314 "scanner_dbt.ml"

  | 17 ->
# 95 "scanner_dbt.mll"
                    ( incr_lineno lexbuf; CR )
# 319 "scanner_dbt.ml"

  | 18 ->
# 99 "scanner_dbt.mll"
                    ( EOF )
# 324 "scanner_dbt.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_drone_basic_rec lexbuf __ocaml_lex_state

;;

