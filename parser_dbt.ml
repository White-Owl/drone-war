type token =
  | CR
  | IF
  | THEN
  | ELSE
  | DO
  | LOOP
  | WHILE
  | UNTIL
  | EXIT
  | SUB
  | FUNCTION
  | CALL
  | END
  | FOR
  | TO
  | STEP
  | NEXT
  | GOTO
  | BOOL of (bool)
  | ID of (string)
  | INT of (int)
  | LPAREN
  | RPAREN
  | COLON
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | EQUAL
  | NOT_EQUAL
  | LESS
  | GREATER
  | LESS_EQUAL
  | GREATER_EQUAL
  | AND
  | OR
  | NOT
  | SLEEP
  | MOVE
  | STOP
  | SHOOT
  | RANDOM
  | HEALTH
  | STARTSCAN
  | NEXTSCAN
  | CANCELSCAN
  | ISEND
  | ISWALL
  | ISFOE
  | ISALLY
  | DISTANCE
  | DIRECTION
  | EOF

open Parsing;;
# 1 "parser_dbt.mly"


open Ast;;
open Printf;;
open Lexing;;
open Utils;;

let auto_label_counter = ref 0;;

let make_label() =
	incr auto_label_counter;
	("-" ^ string_of_int(!auto_label_counter))
	;;


let report_error error_starts_at message =
    raise (Parse_failure (message, error_starts_at.pos_lnum, (error_starts_at.pos_cnum-error_starts_at.pos_bol+1)))
	;;

# 79 "parser_dbt.ml"
let yytransl_const = [|
  257 (* CR *);
  258 (* IF *);
  259 (* THEN *);
  260 (* ELSE *);
  261 (* DO *);
  262 (* LOOP *);
  263 (* WHILE *);
  264 (* UNTIL *);
  265 (* EXIT *);
  266 (* SUB *);
  267 (* FUNCTION *);
  268 (* CALL *);
  269 (* END *);
  270 (* FOR *);
  271 (* TO *);
  272 (* STEP *);
  273 (* NEXT *);
  274 (* GOTO *);
  278 (* LPAREN *);
  279 (* RPAREN *);
  280 (* COLON *);
  281 (* COMMA *);
  282 (* PLUS *);
  283 (* MINUS *);
  284 (* TIMES *);
  285 (* DIVIDE *);
  286 (* EQUAL *);
  287 (* NOT_EQUAL *);
  288 (* LESS *);
  289 (* GREATER *);
  290 (* LESS_EQUAL *);
  291 (* GREATER_EQUAL *);
  292 (* AND *);
  293 (* OR *);
  294 (* NOT *);
  295 (* SLEEP *);
  296 (* MOVE *);
  297 (* STOP *);
  298 (* SHOOT *);
  299 (* RANDOM *);
  300 (* HEALTH *);
  301 (* STARTSCAN *);
  302 (* NEXTSCAN *);
  303 (* CANCELSCAN *);
  304 (* ISEND *);
  305 (* ISWALL *);
  306 (* ISFOE *);
  307 (* ISALLY *);
  308 (* DISTANCE *);
  309 (* DIRECTION *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  275 (* BOOL *);
  276 (* ID *);
  277 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\006\000\006\000\
\006\000\006\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\005\000\005\000\010\000\010\000\010\000\008\000\008\000\
\008\000\009\000\009\000\009\000\009\000\009\000\011\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\012\000\012\000\
\012\000\012\000\012\000\012\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\000\000"

let yylen = "\002\000\
\001\000\000\000\002\000\002\000\002\000\002\000\000\000\002\000\
\002\000\002\000\004\000\003\000\003\000\003\000\002\000\006\000\
\006\000\006\000\005\000\008\000\007\000\006\000\005\000\002\000\
\004\000\007\000\010\000\006\000\005\000\006\000\005\000\009\000\
\011\000\010\000\010\000\000\000\001\000\003\000\000\000\001\000\
\003\000\001\000\003\000\003\000\002\000\001\000\001\000\003\000\
\003\000\006\000\002\000\002\000\002\000\002\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\004\000\001\000\003\000\
\003\000\003\000\003\000\003\000\006\000\003\000\002\000\002\000\
\001\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\074\000\000\000\000\000\003\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\
\005\000\006\000\024\000\000\000\047\000\000\000\061\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\000\000\000\
\000\000\054\000\053\000\051\000\052\000\071\000\072\000\073\000\
\000\000\000\000\045\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\055\000\056\000\057\000\058\000\059\000\060\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\000\000\
\009\000\010\000\012\000\013\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\068\000\048\000\000\000\
\000\000\070\000\000\000\000\000\066\000\067\000\000\000\007\000\
\025\000\043\000\044\000\007\000\007\000\000\000\000\000\037\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\011\000\062\000\000\000\000\000\
\000\000\000\000\000\000\000\000\029\000\031\000\000\000\000\000\
\000\000\000\000\000\000\000\000\019\000\000\000\023\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\000\
\030\000\007\000\038\000\007\000\016\000\017\000\018\000\000\000\
\000\000\000\000\022\000\050\000\069\000\007\000\026\000\000\000\
\000\000\000\000\007\000\000\000\021\000\000\000\000\000\000\000\
\020\000\000\000\000\000\000\000\000\000\000\000\032\000\007\000\
\027\000\034\000\035\000\000\000\033\000"

let yydgoto = "\002\000\
\003\000\004\000\081\000\082\000\018\000\034\000\029\000\101\000\
\030\000\121\000\031\000\073\000"

let yysindex = "\027\000\
\000\000\000\000\000\000\163\255\037\255\000\000\103\255\008\255\
\007\255\024\255\076\255\096\255\081\255\082\255\002\255\000\000\
\000\000\000\000\000\000\000\000\000\000\042\255\000\000\108\255\
\108\255\038\255\092\255\098\255\090\001\118\255\000\255\103\255\
\103\255\205\255\125\255\132\255\126\255\127\255\131\255\133\255\
\138\255\139\255\145\255\148\255\109\255\165\255\000\000\061\255\
\013\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\067\001\156\255\000\000\013\255\013\255\159\255\013\255\013\255\
\013\255\013\255\000\000\000\000\000\000\000\000\000\000\000\000\
\013\255\200\255\108\255\108\255\186\255\189\255\000\000\046\255\
\000\000\000\000\000\000\000\000\134\255\134\255\013\255\013\255\
\013\255\168\255\013\255\169\255\013\255\000\000\244\254\013\255\
\172\255\173\255\083\255\136\001\004\255\000\000\000\000\122\001\
\127\001\000\000\057\255\057\255\000\000\000\000\136\001\000\000\
\000\000\000\000\000\000\000\000\000\000\103\255\103\255\000\000\
\026\255\040\255\043\255\247\254\223\255\198\255\132\001\202\255\
\252\254\113\000\013\255\185\255\000\000\000\000\013\255\013\255\
\013\255\184\255\227\255\201\000\000\000\000\000\212\255\195\255\
\215\255\220\255\221\255\225\255\000\000\013\255\000\000\013\255\
\140\000\229\255\136\001\103\001\110\001\230\255\222\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\117\001\
\071\255\233\255\000\000\000\000\000\000\000\000\000\000\222\000\
\243\000\234\255\000\000\013\255\000\000\008\001\228\255\231\255\
\000\000\029\001\130\255\235\255\242\255\243\255\000\000\000\000\
\000\000\000\000\000\000\050\001\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\240\000\000\000\000\000\000\000\071\001\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\144\000\000\000\080\001\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\180\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\051\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\117\255\117\255\051\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\
\000\000\000\000\000\000\146\255\000\000\000\000\000\000\000\000\
\000\000\000\000\039\000\077\000\000\000\000\000\115\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\153\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\005\000\244\000\000\000\144\255\238\255\166\000\
\226\255\168\000\239\255\000\000"

let yytablesize = 677
let yytable = "\138\000\
\063\000\077\000\078\000\139\000\140\000\057\000\058\000\059\000\
\016\000\049\000\152\000\035\000\056\000\147\000\032\000\033\000\
\063\000\064\000\065\000\066\000\036\000\063\000\064\000\065\000\
\066\000\047\000\134\000\001\000\135\000\099\000\100\000\048\000\
\095\000\023\000\096\000\075\000\076\000\019\000\064\000\054\000\
\055\000\104\000\105\000\037\000\107\000\108\000\109\000\110\000\
\143\000\176\000\144\000\177\000\118\000\119\000\111\000\027\000\
\028\000\114\000\115\000\060\000\056\000\182\000\145\000\049\000\
\144\000\146\000\186\000\135\000\100\000\124\000\125\000\179\000\
\127\000\039\000\129\000\039\000\065\000\130\000\113\000\196\000\
\095\000\023\000\096\000\133\000\065\000\066\000\180\000\141\000\
\142\000\050\000\051\000\052\000\053\000\054\000\055\000\038\000\
\063\000\064\000\065\000\066\000\045\000\046\000\020\000\027\000\
\028\000\097\000\098\000\056\000\063\000\064\000\065\000\066\000\
\153\000\061\000\049\000\039\000\155\000\156\000\157\000\062\000\
\074\000\021\000\022\000\023\000\024\000\083\000\021\000\022\000\
\023\000\024\000\192\000\168\000\084\000\169\000\040\000\041\000\
\042\000\043\000\093\000\036\000\025\000\036\000\044\000\046\000\
\026\000\027\000\028\000\085\000\086\000\026\000\027\000\028\000\
\087\000\120\000\088\000\063\000\064\000\065\000\066\000\089\000\
\090\000\187\000\005\000\006\000\007\000\094\000\091\000\008\000\
\040\000\092\000\040\000\009\000\010\000\011\000\012\000\041\000\
\013\000\041\000\103\000\042\000\014\000\106\000\015\000\005\000\
\079\000\007\000\116\000\158\000\008\000\117\000\126\000\128\000\
\009\000\131\000\132\000\012\000\159\000\013\000\149\000\005\000\
\112\000\014\000\151\000\015\000\005\000\079\000\007\000\154\000\
\009\000\008\000\080\000\012\000\162\000\009\000\163\000\164\000\
\012\000\014\000\013\000\015\000\165\000\166\000\014\000\175\000\
\015\000\167\000\005\000\079\000\007\000\171\000\174\000\008\000\
\160\000\181\000\185\000\009\000\193\000\189\000\012\000\001\000\
\013\000\190\000\194\000\195\000\014\000\148\000\015\000\017\000\
\063\000\064\000\065\000\066\000\123\000\122\000\000\000\000\000\
\063\000\063\000\063\000\063\000\063\000\063\000\063\000\000\000\
\000\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
\063\000\063\000\063\000\000\000\063\000\000\000\000\000\063\000\
\000\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
\063\000\063\000\063\000\063\000\063\000\063\000\064\000\064\000\
\064\000\064\000\064\000\064\000\064\000\000\000\000\000\064\000\
\064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
\064\000\000\000\064\000\000\000\000\000\064\000\000\000\064\000\
\064\000\064\000\000\000\000\000\064\000\064\000\064\000\064\000\
\064\000\064\000\064\000\064\000\065\000\065\000\065\000\065\000\
\065\000\065\000\065\000\000\000\000\000\065\000\065\000\065\000\
\065\000\065\000\065\000\065\000\065\000\065\000\065\000\000\000\
\065\000\000\000\000\000\065\000\000\000\065\000\065\000\065\000\
\000\000\000\000\065\000\065\000\065\000\065\000\065\000\065\000\
\065\000\065\000\049\000\049\000\049\000\049\000\049\000\049\000\
\049\000\000\000\000\000\049\000\049\000\049\000\049\000\049\000\
\049\000\000\000\000\000\049\000\049\000\000\000\049\000\102\000\
\000\000\049\000\063\000\064\000\065\000\066\000\000\000\046\000\
\046\000\046\000\046\000\046\000\046\000\046\000\049\000\049\000\
\046\000\046\000\046\000\046\000\046\000\046\000\000\000\000\000\
\046\000\046\000\170\000\046\000\000\000\063\000\064\000\065\000\
\066\000\073\000\073\000\073\000\073\000\073\000\073\000\073\000\
\073\000\073\000\073\000\042\000\042\000\042\000\042\000\042\000\
\042\000\042\000\000\000\000\000\042\000\042\000\042\000\042\000\
\042\000\042\000\000\000\000\000\042\000\042\000\000\000\042\000\
\005\000\079\000\007\000\000\000\000\000\008\000\161\000\000\000\
\000\000\009\000\000\000\000\000\012\000\000\000\013\000\000\000\
\000\000\000\000\014\000\000\000\015\000\005\000\079\000\007\000\
\000\000\000\000\008\000\000\000\000\000\000\000\009\000\000\000\
\000\000\012\000\183\000\013\000\000\000\000\000\000\000\014\000\
\000\000\015\000\005\000\079\000\007\000\000\000\000\000\008\000\
\000\000\000\000\000\000\009\000\000\000\000\000\012\000\184\000\
\013\000\000\000\000\000\000\000\014\000\000\000\015\000\005\000\
\079\000\007\000\000\000\000\000\008\000\000\000\000\000\000\000\
\009\000\000\000\000\000\012\000\188\000\013\000\000\000\000\000\
\000\000\014\000\000\000\015\000\005\000\079\000\007\000\000\000\
\000\000\008\000\000\000\000\000\000\000\009\000\000\000\000\000\
\012\000\000\000\013\000\000\000\000\000\191\000\014\000\000\000\
\015\000\005\000\079\000\007\000\000\000\000\000\008\000\000\000\
\000\000\000\000\009\000\000\000\000\000\012\000\000\000\013\000\
\000\000\000\000\197\000\014\000\000\000\015\000\007\000\007\000\
\007\000\000\000\000\000\007\000\007\000\000\000\000\000\007\000\
\000\000\000\000\007\000\000\000\007\000\000\000\000\000\000\000\
\007\000\102\000\007\000\000\000\063\000\064\000\065\000\066\000\
\067\000\068\000\069\000\070\000\071\000\072\000\063\000\000\000\
\000\000\063\000\063\000\063\000\063\000\063\000\063\000\063\000\
\063\000\063\000\063\000\063\000\064\000\065\000\066\000\067\000\
\068\000\069\000\070\000\071\000\072\000\172\000\000\000\000\000\
\063\000\064\000\065\000\066\000\173\000\000\000\000\000\063\000\
\064\000\065\000\066\000\178\000\000\000\000\000\063\000\064\000\
\065\000\066\000\136\000\063\000\064\000\065\000\066\000\137\000\
\063\000\064\000\065\000\066\000\150\000\063\000\064\000\065\000\
\066\000\063\000\064\000\065\000\066\000"

let yycheck = "\112\000\
\000\000\032\000\033\000\116\000\117\000\024\000\024\000\025\000\
\004\000\022\001\015\001\005\001\000\001\023\001\007\001\008\001\
\026\001\027\001\028\001\029\001\014\001\026\001\027\001\028\001\
\029\001\024\001\023\001\001\000\025\001\048\000\049\000\030\001\
\020\001\021\001\022\001\036\001\037\001\001\001\000\000\052\001\
\053\001\060\000\061\000\020\001\063\000\064\000\065\000\066\000\
\023\001\162\000\025\001\164\000\007\001\008\001\073\000\043\001\
\044\001\075\000\076\000\022\001\000\001\174\000\023\001\022\001\
\025\001\023\001\179\000\025\001\087\000\088\000\089\000\001\001\
\091\000\023\001\093\000\025\001\000\000\096\000\074\000\192\000\
\020\001\021\001\022\001\001\001\028\001\029\001\016\001\118\000\
\119\000\048\001\049\001\050\001\051\001\052\001\053\001\020\001\
\026\001\027\001\028\001\029\001\020\001\020\001\000\001\043\001\
\044\001\045\001\046\001\000\001\026\001\027\001\028\001\029\001\
\131\000\022\001\000\000\020\001\135\000\136\000\137\000\022\001\
\003\001\019\001\020\001\021\001\022\001\001\001\019\001\020\001\
\021\001\022\001\001\001\150\000\001\001\152\000\039\001\040\001\
\041\001\042\001\030\001\023\001\038\001\025\001\047\001\000\000\
\042\001\043\001\044\001\022\001\022\001\042\001\043\001\044\001\
\022\001\020\001\022\001\026\001\027\001\028\001\029\001\022\001\
\022\001\180\000\000\001\001\001\002\001\001\001\022\001\005\001\
\023\001\022\001\025\001\009\001\010\001\011\001\012\001\023\001\
\014\001\025\001\023\001\000\000\018\001\023\001\020\001\000\001\
\001\001\002\001\001\001\004\001\005\001\001\001\023\001\023\001\
\009\001\022\001\022\001\012\001\013\001\014\001\001\001\000\001\
\001\001\018\001\001\001\020\001\000\001\001\001\002\001\023\001\
\009\001\005\001\006\001\012\001\001\001\009\001\020\001\001\001\
\012\001\018\001\014\001\020\001\001\001\001\001\018\001\002\001\
\020\001\001\001\000\001\001\001\002\001\001\001\001\001\005\001\
\006\001\001\001\001\001\009\001\002\001\010\001\012\001\000\000\
\014\001\011\001\001\001\001\001\018\001\023\001\020\001\004\000\
\026\001\027\001\028\001\029\001\087\000\086\000\255\255\255\255\
\000\001\001\001\002\001\003\001\004\001\005\001\006\001\255\255\
\255\255\009\001\010\001\011\001\012\001\013\001\014\001\015\001\
\016\001\017\001\018\001\255\255\020\001\255\255\255\255\023\001\
\255\255\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\000\001\001\001\
\002\001\003\001\004\001\005\001\006\001\255\255\255\255\009\001\
\010\001\011\001\012\001\013\001\014\001\015\001\016\001\017\001\
\018\001\255\255\020\001\255\255\255\255\023\001\255\255\025\001\
\026\001\027\001\255\255\255\255\030\001\031\001\032\001\033\001\
\034\001\035\001\036\001\037\001\000\001\001\001\002\001\003\001\
\004\001\005\001\006\001\255\255\255\255\009\001\010\001\011\001\
\012\001\013\001\014\001\015\001\016\001\017\001\018\001\255\255\
\020\001\255\255\255\255\023\001\255\255\025\001\026\001\027\001\
\255\255\255\255\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001\000\001\001\001\002\001\003\001\004\001\005\001\
\006\001\255\255\255\255\009\001\010\001\011\001\012\001\013\001\
\014\001\255\255\255\255\017\001\018\001\255\255\020\001\023\001\
\255\255\023\001\026\001\027\001\028\001\029\001\255\255\000\001\
\001\001\002\001\003\001\004\001\005\001\006\001\036\001\037\001\
\009\001\010\001\011\001\012\001\013\001\014\001\255\255\255\255\
\017\001\018\001\023\001\020\001\255\255\026\001\027\001\028\001\
\029\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\000\001\001\001\002\001\003\001\004\001\
\005\001\006\001\255\255\255\255\009\001\010\001\011\001\012\001\
\013\001\014\001\255\255\255\255\017\001\018\001\255\255\020\001\
\000\001\001\001\002\001\255\255\255\255\005\001\006\001\255\255\
\255\255\009\001\255\255\255\255\012\001\255\255\014\001\255\255\
\255\255\255\255\018\001\255\255\020\001\000\001\001\001\002\001\
\255\255\255\255\005\001\255\255\255\255\255\255\009\001\255\255\
\255\255\012\001\013\001\014\001\255\255\255\255\255\255\018\001\
\255\255\020\001\000\001\001\001\002\001\255\255\255\255\005\001\
\255\255\255\255\255\255\009\001\255\255\255\255\012\001\013\001\
\014\001\255\255\255\255\255\255\018\001\255\255\020\001\000\001\
\001\001\002\001\255\255\255\255\005\001\255\255\255\255\255\255\
\009\001\255\255\255\255\012\001\013\001\014\001\255\255\255\255\
\255\255\018\001\255\255\020\001\000\001\001\001\002\001\255\255\
\255\255\005\001\255\255\255\255\255\255\009\001\255\255\255\255\
\012\001\255\255\014\001\255\255\255\255\017\001\018\001\255\255\
\020\001\000\001\001\001\002\001\255\255\255\255\005\001\255\255\
\255\255\255\255\009\001\255\255\255\255\012\001\255\255\014\001\
\255\255\255\255\017\001\018\001\255\255\020\001\000\001\001\001\
\002\001\255\255\255\255\005\001\006\001\255\255\255\255\009\001\
\255\255\255\255\012\001\255\255\014\001\255\255\255\255\255\255\
\018\001\023\001\020\001\255\255\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\023\001\255\255\
\255\255\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\026\001\027\001\028\001\029\001\030\001\
\031\001\032\001\033\001\034\001\035\001\023\001\255\255\255\255\
\026\001\027\001\028\001\029\001\023\001\255\255\255\255\026\001\
\027\001\028\001\029\001\023\001\255\255\255\255\026\001\027\001\
\028\001\029\001\025\001\026\001\027\001\028\001\029\001\025\001\
\026\001\027\001\028\001\029\001\025\001\026\001\027\001\028\001\
\029\001\026\001\027\001\028\001\029\001"

let yynames_const = "\
  CR\000\
  IF\000\
  THEN\000\
  ELSE\000\
  DO\000\
  LOOP\000\
  WHILE\000\
  UNTIL\000\
  EXIT\000\
  SUB\000\
  FUNCTION\000\
  CALL\000\
  END\000\
  FOR\000\
  TO\000\
  STEP\000\
  NEXT\000\
  GOTO\000\
  LPAREN\000\
  RPAREN\000\
  COLON\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  EQUAL\000\
  NOT_EQUAL\000\
  LESS\000\
  GREATER\000\
  LESS_EQUAL\000\
  GREATER_EQUAL\000\
  AND\000\
  OR\000\
  NOT\000\
  SLEEP\000\
  MOVE\000\
  STOP\000\
  SHOOT\000\
  RANDOM\000\
  HEALTH\000\
  STARTSCAN\000\
  NEXTSCAN\000\
  CANCELSCAN\000\
  ISEND\000\
  ISWALL\000\
  ISFOE\000\
  ISALLY\000\
  DISTANCE\000\
  DIRECTION\000\
  EOF\000\
  "

let yynames_block = "\
  BOOL\000\
  ID\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 55 "parser_dbt.mly"
          ( let main_sub = { name="--"; body = List.rev (fst _1); } in
			main_sub :: snd _1 )
# 496 "parser_dbt.ml"
               : Ast.sub list))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser_dbt.mly"
         ([],[])
# 502 "parser_dbt.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program) in
    Obj.repr(
# 60 "parser_dbt.mly"
                                 ( _1 )
# 509 "parser_dbt.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 61 "parser_dbt.mly"
                                 ( (_2 @ fst _1), snd _1 )
# 517 "parser_dbt.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'compaund_statement) in
    Obj.repr(
# 62 "parser_dbt.mly"
                                 ( (_2 @ fst _1), snd _1 )
# 525 "parser_dbt.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sub) in
    Obj.repr(
# 63 "parser_dbt.mly"
                                 ( fst _1, (_2 :: snd _1) )
# 533 "parser_dbt.ml"
               : 'program))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser_dbt.mly"
                                  ( [] )
# 539 "parser_dbt.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 68 "parser_dbt.mly"
                                  ( _1 )
# 546 "parser_dbt.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 69 "parser_dbt.mly"
                                  ( _2 @ _1 )
# 554 "parser_dbt.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'compaund_statement) in
    Obj.repr(
# 70 "parser_dbt.mly"
                                  ( _2 @ _1 )
# 562 "parser_dbt.ml"
               : 'statements))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'math_expr) in
    Obj.repr(
# 74 "parser_dbt.mly"
                                                          ( Store(_1) :: _3 )
# 570 "parser_dbt.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser_dbt.mly"
                                                          ( [ Jump("--ExitDo") ] )
# 576 "parser_dbt.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser_dbt.mly"
                                                          ( [ Jump("--ExitFor") ] )
# 582 "parser_dbt.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 77 "parser_dbt.mly"
                                                          ( [ Jump(_2) ] )
# 589 "parser_dbt.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 78 "parser_dbt.mly"
                                                          ( [ Label(_1) ] )
# 596 "parser_dbt.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'parameters) in
    Obj.repr(
# 79 "parser_dbt.mly"
                                                          ( Call(_2) :: _4 )
# 604 "parser_dbt.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'math_expr) in
    Obj.repr(
# 80 "parser_dbt.mly"
                                                          ( Wait :: _4 )
# 611 "parser_dbt.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'math_expr) in
    Obj.repr(
# 81 "parser_dbt.mly"
                                                          ( Move :: _4 )
# 618 "parser_dbt.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser_dbt.mly"
                                                         ( [ Stop ] )
# 624 "parser_dbt.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'math_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'math_expr) in
    Obj.repr(
# 83 "parser_dbt.mly"
                                                          ( Drop :: Shoot :: (_4 @ _6) )
# 632 "parser_dbt.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'math_expr) in
    Obj.repr(
# 84 "parser_dbt.mly"
                                                          ( [ Store(_1^".distance"); Store(_1^".direction"); Store(_1^".flag"); Look ] @ _5 )
# 640 "parser_dbt.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    Obj.repr(
# 85 "parser_dbt.mly"
                                                          ( [ Store(_1^".distance"); Store(_1^".direction"); Store(_1^".flag"); ] )
# 647 "parser_dbt.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser_dbt.mly"
                                                          ( let lblAgain=make_label() and lblDone=make_label() in
                                                            [ Label(lblDone); Jump(lblAgain); Drop; Drop; JumpIf(lblDone); IsEnd;  Label(lblAgain)] )
# 654 "parser_dbt.ml"
               : 'statement))
; (fun __caml_parser_env ->
    Obj.repr(
# 88 "parser_dbt.mly"
                                                          ( report_error (Parsing.rhs_start_pos 1) "Syntax error" )
# 660 "parser_dbt.ml"
               : 'statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'condition) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'statement) in
    Obj.repr(
# 93 "parser_dbt.mly"
  ( let lbl = make_label() in
		  Label(lbl) :: ( _4 @ ( [ JumpIf(lbl) ; Not ]  @ _2 ) )
		)
# 670 "parser_dbt.ml"
               : 'compaund_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : 'condition) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'statements) in
    Obj.repr(
# 97 "parser_dbt.mly"
  ( let lbl = make_label() in
		  Label(lbl) :: ( _5 @ ( [ JumpIf(lbl) ; Not ]  @ _2 ) )
		)
# 680 "parser_dbt.ml"
               : 'compaund_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'condition) in
    let _5 = (Parsing.peek_val __caml_parser_env 5 : 'statements) in
    let _8 = (Parsing.peek_val __caml_parser_env 2 : 'statements) in
    Obj.repr(
# 101 "parser_dbt.mly"
  ( let lblTrue = make_label() in
		  let lblEndIf = make_label() in
		  Label(lblEndIf) :: ( _5 @ (Label(lblTrue) :: Jump(lblEndIf) :: ( _8 @ ( JumpIf(lblTrue) :: _2) ) ) )
		)
# 692 "parser_dbt.ml"
               : 'compaund_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 106 "parser_dbt.mly"
  ( let lblStart = make_label() and lblCheckCondition = make_label() and lblDone = make_label() in
		  let block = List.map (fun x -> match x with Jump("--ExitDo") -> Jump(lblDone) | _ -> x) _5 in
		  Label(lblDone) :: JumpIf(lblStart) :: (_3 @ (Label(lblCheckCondition) :: (block @ [Label(lblStart); Jump(lblCheckCondition) ])))
		)
# 703 "parser_dbt.ml"
               : 'compaund_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'statements) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 111 "parser_dbt.mly"
  ( let lblStart = make_label() and lblDone = make_label() in
		  let block = List.map (fun x -> match x with Jump("--ExitDo") -> Jump(lblDone) | _ -> x) _2 in
		  Label(lblDone) :: JumpIf(lblStart) :: (_5 @ (block @ [Label(lblStart)]))
		)
# 714 "parser_dbt.ml"
               : 'compaund_statement))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'condition) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 116 "parser_dbt.mly"
  ( let lblCheckCondition = make_label() and lblDone = make_label() in
		  let block = List.map (fun x -> match x with Jump("--ExitDo") -> Jump(lblDone) | _ -> x) _5 in
		  Label(lblDone) :: Jump(lblCheckCondition) :: (block @ ( JumpIf(lblDone) :: (_3 @ [Label(lblCheckCondition)])))
		)
# 725 "parser_dbt.ml"
               : 'compaund_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'statements) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'condition) in
    Obj.repr(
# 121 "parser_dbt.mly"
  ( let lblStart = make_label() and lblDone = make_label() in
		  let block = List.map (fun x -> match x with Jump("--ExitDo") -> Jump(lblDone) | _ -> x) _2 in
		  Label(lblDone) :: JumpIf(lblStart) :: Not :: (_5 @ (block @ [Label(lblStart)]))
		)
# 736 "parser_dbt.ml"
               : 'compaund_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'math_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'math_expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 126 "parser_dbt.mly"
  ( let lblAgain = make_label() and lblDone = make_label() in
		  let block = List.map (fun x -> match x with Jump("--ExitFor") -> Jump(lblDone) | _ -> x) _8 in
		  [Label(lblDone); JumpIf(lblAgain); Less] @ _6 @ [ Store(_2); Dup; Plus; Int(1); Read(_2)] @ block @ [Label(lblAgain); Store(_2)] @ _4
		)
# 749 "parser_dbt.ml"
               : 'compaund_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 9 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 7 : 'math_expr) in
    let _6 = (Parsing.peek_val __caml_parser_env 5 : 'math_expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 3 : 'math_expr) in
    let _10 = (Parsing.peek_val __caml_parser_env 1 : 'statements) in
    Obj.repr(
# 131 "parser_dbt.mly"
  ( let lblAgain = make_label() and lblDone = make_label() in
		  let block = List.map (fun x -> match x with Jump("--ExitFor") -> Jump(lblDone) | _ -> x) _10 in
		  [Label(lblDone); JumpIf(lblAgain); Less] @ _6 @ [ Store(_2); Dup; Plus] @ _8 @ [Read(_2)] @ block @ [Label(lblAgain); Store(_2)] @ _4
		)
# 763 "parser_dbt.ml"
               : 'compaund_statement))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'statements) in
    Obj.repr(
# 138 "parser_dbt.mly"
  ( let read_arguments = List.map (fun arg -> Store(arg)) _4 in
		  let sub_body = List.map(fun x -> match x with
		                            Read(name) -> if List.exists (fun arg -> arg=name) _4 then Read(_2^"-"^name) else Read(name)
		                          | Store(name) -> if List.exists (fun arg -> arg=name) _4 then Store(_2^"-"^name) else Store(name)
		                          | _ -> x) (_7 @ read_arguments) in
		  { name = _2; body = List.rev sub_body; }
		)
# 778 "parser_dbt.ml"
               : 'sub))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'args) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'statements) in
    Obj.repr(
# 146 "parser_dbt.mly"
  ( let read_arguments = List.map (fun arg -> Store(arg)) _4 in
		  let sub_body = List.map(fun x -> match x with
		                            Read(name) -> if List.exists (fun arg -> arg=name) _4 then Read(_2^"-"^name) else Read(name)
		                          | Store(name) -> if List.exists (fun arg -> arg=name) _4 then Store(_2^"-"^name) else if name=_2 then Store(_2^"-") else Store(name)
		                          | _ -> x) (_7 @ read_arguments) in
		  { name = _2; body = List.rev (Read(_2^"-") :: sub_body); }
		)
# 793 "parser_dbt.ml"
               : 'sub))
; (fun __caml_parser_env ->
    Obj.repr(
# 154 "parser_dbt.mly"
      ( [] )
# 799 "parser_dbt.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 155 "parser_dbt.mly"
                  ( [ _1 ] )
# 806 "parser_dbt.ml"
               : 'args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 156 "parser_dbt.mly"
                  ( _3 :: _1 )
# 814 "parser_dbt.ml"
               : 'args))
; (fun __caml_parser_env ->
    Obj.repr(
# 159 "parser_dbt.mly"
            ( [] )
# 820 "parser_dbt.ml"
               : 'parameters))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'math_expr) in
    Obj.repr(
# 160 "parser_dbt.mly"
              ( _1 )
# 827 "parser_dbt.ml"
               : 'parameters))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'parameters) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'math_expr) in
    Obj.repr(
# 161 "parser_dbt.mly"
                               ( _3 @ _1 )
# 835 "parser_dbt.ml"
               : 'parameters))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'logic_expr) in
    Obj.repr(
# 164 "parser_dbt.mly"
                                   ( _1 )
# 842 "parser_dbt.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'logic_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'logic_expr) in
    Obj.repr(
# 165 "parser_dbt.mly"
                                   ( And :: (_3 @ _1) )
# 850 "parser_dbt.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'logic_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'logic_expr) in
    Obj.repr(
# 166 "parser_dbt.mly"
                                   ( Or :: (_3 @ _1) )
# 858 "parser_dbt.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'logic_expr) in
    Obj.repr(
# 167 "parser_dbt.mly"
                                   ( Not :: _2 )
# 865 "parser_dbt.ml"
               : 'condition))
; (fun __caml_parser_env ->
    Obj.repr(
# 168 "parser_dbt.mly"
                                   ( report_error (Parsing.rhs_start_pos 1) "Malformed logical expression" )
# 871 "parser_dbt.ml"
               : 'condition))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 172 "parser_dbt.mly"
                                                   ( [ Bool(_1) ] )
# 878 "parser_dbt.ml"
               : 'logic_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'logic_expr) in
    Obj.repr(
# 173 "parser_dbt.mly"
                                                   ( _2 )
# 885 "parser_dbt.ml"
               : 'logic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'math_expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'math_relation) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'math_expr) in
    Obj.repr(
# 174 "parser_dbt.mly"
                                                   ( _2 @ ( _3 @ _1) )
# 894 "parser_dbt.ml"
               : 'logic_expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'math_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'math_expr) in
    Obj.repr(
# 175 "parser_dbt.mly"
                                                   ( Shoot :: (_3 @ _5) )
# 902 "parser_dbt.ml"
               : 'logic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 176 "parser_dbt.mly"
                                                   ( [ IsFoe; Read(_1^".flag") ] )
# 909 "parser_dbt.ml"
               : 'logic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 177 "parser_dbt.mly"
                                                   ( [ IsAlly; Read(_1^".flag") ] )
# 916 "parser_dbt.ml"
               : 'logic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 178 "parser_dbt.mly"
                                                   ( [ IsWall; Read(_1^".flag") ] )
# 923 "parser_dbt.ml"
               : 'logic_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 179 "parser_dbt.mly"
                                                   ( [ IsEnd; Read(_1^".flag") ] )
# 930 "parser_dbt.ml"
               : 'logic_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 183 "parser_dbt.mly"
                  ( [ Equal ] )
# 936 "parser_dbt.ml"
               : 'math_relation))
; (fun __caml_parser_env ->
    Obj.repr(
# 184 "parser_dbt.mly"
                  ( [ Equal ; Not ] )
# 942 "parser_dbt.ml"
               : 'math_relation))
; (fun __caml_parser_env ->
    Obj.repr(
# 185 "parser_dbt.mly"
                  ( [ Less ] )
# 948 "parser_dbt.ml"
               : 'math_relation))
; (fun __caml_parser_env ->
    Obj.repr(
# 186 "parser_dbt.mly"
                  ( [ Greater ] )
# 954 "parser_dbt.ml"
               : 'math_relation))
; (fun __caml_parser_env ->
    Obj.repr(
# 187 "parser_dbt.mly"
                  ( [ Greater ; Not ] )
# 960 "parser_dbt.ml"
               : 'math_relation))
; (fun __caml_parser_env ->
    Obj.repr(
# 188 "parser_dbt.mly"
                  ( [ Less ; Not ] )
# 966 "parser_dbt.ml"
               : 'math_relation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 192 "parser_dbt.mly"
                                 ( [ Int(_1) ] )
# 973 "parser_dbt.ml"
               : 'math_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'parameters) in
    Obj.repr(
# 193 "parser_dbt.mly"
                                 ( Call(_1) :: _3 )
# 981 "parser_dbt.ml"
               : 'math_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 194 "parser_dbt.mly"
                                 ( [ Read(_1) ] )
# 988 "parser_dbt.ml"
               : 'math_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'math_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'math_expr) in
    Obj.repr(
# 195 "parser_dbt.mly"
                                 ( Plus :: ( _3 @ _1) )
# 996 "parser_dbt.ml"
               : 'math_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'math_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'math_expr) in
    Obj.repr(
# 196 "parser_dbt.mly"
                                 ( Minus :: ( _3 @ _1) )
# 1004 "parser_dbt.ml"
               : 'math_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'math_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'math_expr) in
    Obj.repr(
# 197 "parser_dbt.mly"
                                 ( Times :: ( _3 @ _1) )
# 1012 "parser_dbt.ml"
               : 'math_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'math_expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'math_expr) in
    Obj.repr(
# 198 "parser_dbt.mly"
                                 ( Divide :: ( _3 @ _1) )
# 1020 "parser_dbt.ml"
               : 'math_expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'math_expr) in
    Obj.repr(
# 199 "parser_dbt.mly"
                                 ( _2 )
# 1027 "parser_dbt.ml"
               : 'math_expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'math_expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'math_expr) in
    Obj.repr(
# 200 "parser_dbt.mly"
                                                   ( Random :: (_5 @ _3) )
# 1035 "parser_dbt.ml"
               : 'math_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 201 "parser_dbt.mly"
                                 ( [ GetHealth ] )
# 1041 "parser_dbt.ml"
               : 'math_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 202 "parser_dbt.mly"
                                 ( [ Read(_1^".distance") ] )
# 1048 "parser_dbt.ml"
               : 'math_expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 203 "parser_dbt.mly"
                                 ( [ Read(_1^".direction") ] )
# 1055 "parser_dbt.ml"
               : 'math_expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 204 "parser_dbt.mly"
                                 ( report_error (Parsing.rhs_start_pos 1) "Malformed math expression" )
# 1061 "parser_dbt.ml"
               : 'math_expr))
(* Entry drone *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let drone (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.sub list)
