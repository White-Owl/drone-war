type token =
  | SUB
  | END_SUB
  | IF
  | ELSE
  | END_IF
  | BEGIN
  | WHILE
  | AGAIN
  | READ
  | STORE
  | COLON
  | JUMP
  | JUMPIF
  | LABEL of (string)
  | NAME of (string)
  | INTEGER of (int)
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | POWER
  | AND
  | OR
  | NOT
  | BOOL of (bool)
  | EQUAL
  | LESS
  | GREATER
  | DROP
  | DROPALL
  | DUP
  | SWAP
  | OVER
  | ROT
  | MOVE
  | STOP
  | SHOOT
  | LOOK
  | ISFOE
  | ISALLY
  | ISWALL
  | ISEND
  | WAIT
  | GETHEALTH
  | RANDOM
  | EOF

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"


open Ast;;
open Printf;;
open Lexing;;
open Utils;;

let auto_label_counter = ref 0;;

let make_label() =
	incr auto_label_counter;
	("-" ^ string_of_int(!auto_label_counter))
	;;

# 68 "parser.ml"
let yytransl_const = [|
  257 (* SUB *);
  258 (* END_SUB *);
  259 (* IF *);
  260 (* ELSE *);
  261 (* END_IF *);
  262 (* BEGIN *);
  263 (* WHILE *);
  264 (* AGAIN *);
  265 (* READ *);
  266 (* STORE *);
  267 (* COLON *);
  268 (* JUMP *);
  269 (* JUMPIF *);
  273 (* PLUS *);
  274 (* MINUS *);
  275 (* TIMES *);
  276 (* DIVIDE *);
  277 (* MOD *);
  278 (* POWER *);
  279 (* AND *);
  280 (* OR *);
  281 (* NOT *);
  283 (* EQUAL *);
  284 (* LESS *);
  285 (* GREATER *);
  286 (* DROP *);
  287 (* DROPALL *);
  288 (* DUP *);
  289 (* SWAP *);
  290 (* OVER *);
  291 (* ROT *);
  292 (* MOVE *);
  293 (* STOP *);
  294 (* SHOOT *);
  295 (* LOOK *);
  296 (* ISFOE *);
  297 (* ISALLY *);
  298 (* ISWALL *);
  299 (* ISEND *);
  300 (* WAIT *);
  301 (* GETHEALTH *);
  302 (* RANDOM *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  270 (* LABEL *);
  271 (* NAME *);
  272 (* INTEGER *);
  282 (* BOOL *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\004\000\006\000\006\000\
\006\000\006\000\005\000\005\000\005\000\005\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\000\000"

let yylen = "\002\000\
\001\000\000\000\002\000\002\000\002\000\004\000\000\000\002\000\
\002\000\002\000\003\000\005\000\003\000\005\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\001\000\002\000\002\000\001\000\001\000\
\001\000\001\000\001\000\001\000\001\000\002\000\002\000\001\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\001\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\052\000\000\000\000\000\007\000\007\000\037\000\
\000\000\015\000\016\000\017\000\018\000\019\000\020\000\021\000\
\022\000\023\000\024\000\025\000\026\000\027\000\028\000\031\000\
\032\000\033\000\034\000\035\000\036\000\041\000\042\000\043\000\
\044\000\045\000\046\000\047\000\048\000\049\000\050\000\051\000\
\003\000\004\000\005\000\007\000\000\000\000\000\029\000\030\000\
\038\000\039\000\000\000\010\000\007\000\011\000\008\000\009\000\
\007\000\013\000\006\000\000\000\000\000\012\000\014\000"

let yydgoto = "\002\000\
\003\000\004\000\055\000\042\000\056\000\045\000"

let yysindex = "\003\000\
\000\000\000\000\000\000\094\000\248\254\000\000\000\000\000\000\
\249\254\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\038\255\085\255\000\000\000\000\
\000\000\000\000\132\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\179\255\048\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\008\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\005\000\000\000\006\000\249\255"

let yytablesize = 396
let yytable = "\046\000\
\040\000\047\000\048\000\001\000\049\000\050\000\044\000\001\000\
\041\000\043\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\051\000\052\000\000\000\000\000\
\006\000\053\000\054\000\007\000\000\000\060\000\000\000\000\000\
\000\000\061\000\000\000\008\000\009\000\010\000\011\000\012\000\
\013\000\014\000\015\000\016\000\017\000\018\000\019\000\020\000\
\021\000\022\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\036\000\
\037\000\038\000\039\000\040\000\052\000\000\000\000\000\006\000\
\000\000\000\000\007\000\057\000\058\000\000\000\000\000\000\000\
\000\000\000\000\008\000\009\000\010\000\011\000\012\000\013\000\
\014\000\015\000\016\000\017\000\018\000\019\000\020\000\021\000\
\022\000\023\000\024\000\025\000\026\000\027\000\028\000\029\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\040\000\052\000\000\000\059\000\006\000\000\000\
\000\000\007\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\008\000\009\000\010\000\011\000\012\000\013\000\014\000\
\015\000\016\000\017\000\018\000\019\000\020\000\021\000\022\000\
\023\000\024\000\025\000\026\000\027\000\028\000\029\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\040\000\052\000\000\000\000\000\006\000\000\000\062\000\
\007\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\008\000\009\000\010\000\011\000\012\000\013\000\014\000\015\000\
\016\000\017\000\018\000\019\000\020\000\021\000\022\000\023\000\
\024\000\025\000\026\000\027\000\028\000\029\000\030\000\031\000\
\032\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\040\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\040\000\000\000\000\000\000\000\000\000\000\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\052\000\
\000\000\000\000\006\000\000\000\000\000\007\000\000\000\063\000\
\000\000\000\000\000\000\000\000\000\000\008\000\009\000\010\000\
\011\000\012\000\013\000\014\000\015\000\016\000\017\000\018\000\
\019\000\020\000\021\000\022\000\023\000\024\000\025\000\026\000\
\027\000\028\000\029\000\030\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\039\000\040\000\005\000\000\000\
\006\000\000\000\000\000\007\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\008\000\009\000\010\000\011\000\012\000\
\013\000\014\000\015\000\016\000\017\000\018\000\019\000\020\000\
\021\000\022\000\023\000\024\000\025\000\026\000\027\000\028\000\
\029\000\030\000\031\000\032\000\033\000\034\000\035\000\036\000\
\037\000\038\000\039\000\040\000"

let yycheck = "\007\000\
\000\000\009\001\010\001\001\000\012\001\013\001\015\001\000\000\
\004\000\004\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\044\000\000\001\255\255\255\255\
\003\001\004\001\005\001\006\001\255\255\053\000\255\255\255\255\
\255\255\057\000\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\041\001\042\001\
\043\001\044\001\045\001\046\001\000\001\255\255\255\255\003\001\
\255\255\255\255\006\001\007\001\008\001\255\255\255\255\255\255\
\255\255\255\255\014\001\015\001\016\001\017\001\018\001\019\001\
\020\001\021\001\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\032\001\033\001\034\001\035\001\
\036\001\037\001\038\001\039\001\040\001\041\001\042\001\043\001\
\044\001\045\001\046\001\000\001\255\255\002\001\003\001\255\255\
\255\255\006\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\014\001\015\001\016\001\017\001\018\001\019\001\020\001\
\021\001\022\001\023\001\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\032\001\033\001\034\001\035\001\036\001\
\037\001\038\001\039\001\040\001\041\001\042\001\043\001\044\001\
\045\001\046\001\000\001\255\255\255\255\003\001\255\255\005\001\
\006\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\032\001\033\001\034\001\035\001\036\001\037\001\
\038\001\039\001\040\001\041\001\042\001\043\001\044\001\045\001\
\046\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\000\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\255\255\255\255\255\255\255\255\255\255\014\001\015\001\
\016\001\017\001\018\001\019\001\020\001\021\001\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\032\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\041\001\042\001\043\001\044\001\045\001\046\001\000\001\
\255\255\255\255\003\001\255\255\255\255\006\001\255\255\008\001\
\255\255\255\255\255\255\255\255\255\255\014\001\015\001\016\001\
\017\001\018\001\019\001\020\001\021\001\022\001\023\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\032\001\
\033\001\034\001\035\001\036\001\037\001\038\001\039\001\040\001\
\041\001\042\001\043\001\044\001\045\001\046\001\001\001\255\255\
\003\001\255\255\255\255\006\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\014\001\015\001\016\001\017\001\018\001\
\019\001\020\001\021\001\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\032\001\033\001\034\001\
\035\001\036\001\037\001\038\001\039\001\040\001\041\001\042\001\
\043\001\044\001\045\001\046\001"

let yynames_const = "\
  SUB\000\
  END_SUB\000\
  IF\000\
  ELSE\000\
  END_IF\000\
  BEGIN\000\
  WHILE\000\
  AGAIN\000\
  READ\000\
  STORE\000\
  COLON\000\
  JUMP\000\
  JUMPIF\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  MOD\000\
  POWER\000\
  AND\000\
  OR\000\
  NOT\000\
  EQUAL\000\
  LESS\000\
  GREATER\000\
  DROP\000\
  DROPALL\000\
  DUP\000\
  SWAP\000\
  OVER\000\
  ROT\000\
  MOVE\000\
  STOP\000\
  SHOOT\000\
  LOOK\000\
  ISFOE\000\
  ISALLY\000\
  ISWALL\000\
  ISEND\000\
  WAIT\000\
  GETHEALTH\000\
  RANDOM\000\
  EOF\000\
  "

let yynames_block = "\
  LABEL\000\
  NAME\000\
  INTEGER\000\
  BOOL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'program) in
    Obj.repr(
# 40 "parser.mly"
          ( let main_sub = { name="--"; body = List.rev (fst _1); } in
			main_sub :: snd _1 )
# 342 "parser.ml"
               : Ast.sub list))
; (fun __caml_parser_env ->
    Obj.repr(
# 44 "parser.mly"
 ( [], [] )
# 348 "parser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'operation) in
    Obj.repr(
# 45 "parser.mly"
                     ( (_2 :: fst _1), snd _1 )
# 356 "parser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'sub) in
    Obj.repr(
# 46 "parser.mly"
               ( fst _1, (_2 :: snd _1) )
# 364 "parser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'compaund_statment) in
    Obj.repr(
# 47 "parser.mly"
                             ( (_2 @ fst _1), snd _1 )
# 372 "parser.ml"
               : 'program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'operations) in
    Obj.repr(
# 50 "parser.mly"
                               ( { name = _2; body = List.rev _3; } )
# 380 "parser.ml"
               : 'sub))
; (fun __caml_parser_env ->
    Obj.repr(
# 54 "parser.mly"
 ( [] )
# 386 "parser.ml"
               : 'operations))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'operations) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'operation) in
    Obj.repr(
# 55 "parser.mly"
                          ( if _2=Nop then _1 else _2 :: _1 )
# 394 "parser.ml"
               : 'operations))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'operations) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'compaund_statment) in
    Obj.repr(
# 56 "parser.mly"
                                   ( _2 @ _1 )
# 402 "parser.ml"
               : 'operations))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'operations) in
    Obj.repr(
# 57 "parser.mly"
                          ( let pos = Parsing.rhs_start_pos 2 in
	                           raise (Parse_failure ("Unrecognized tokens starting from line %d position %d\n", pos.pos_lnum, (pos.pos_cnum - pos.pos_bol +1)));
	                           (* TO DO! I have no idea how to reach lex_buffer from the parser. So the exact token or text of the line are unknown *)
	                         )
# 412 "parser.ml"
               : 'operations))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'operations) in
    Obj.repr(
# 64 "parser.mly"
                          (  let lbl = make_label() in
								( Label(lbl):: _2 ) @ [ JumpIf(lbl) ; Not ]
							 )
# 421 "parser.ml"
               : 'compaund_statment))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'operations) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'operations) in
    Obj.repr(
# 67 "parser.mly"
                                        ( let lbl1 = make_label() and lbl2= make_label() in
								 ( Label(lbl2):: _4) @ ( Label(lbl1)::(Jump(lbl2):: _2 )) @ [ JumpIf(lbl1) ; Not ]  
							 )
# 431 "parser.ml"
               : 'compaund_statment))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'operations) in
    Obj.repr(
# 70 "parser.mly"
                          ( let lbl=make_label() in
								(Jump(lbl)::_2) @ [Label(lbl)]
							)
# 440 "parser.ml"
               : 'compaund_statment))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : 'operations) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'operations) in
    Obj.repr(
# 73 "parser.mly"
                                           (let lbl1 =make_label() and lbl2 = make_label() in
								[Label(lbl1); Jump(lbl2)] @ _4 @ [ JumpIf(lbl1) ; Not ] @ _2 @ [Label(lbl2)]
							)
# 450 "parser.ml"
               : 'compaund_statment))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 78 "parser.mly"
                 ( Int(_1) )
# 457 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 79 "parser.mly"
                 ( Plus )
# 463 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
                 ( Minus )
# 469 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
                 ( Times )
# 475 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
                 ( Divide )
# 481 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
                 ( Mod )
# 487 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "parser.mly"
                 ( Power )
# 493 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 85 "parser.mly"
                 ( And )
# 499 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 86 "parser.mly"
                 ( Or )
# 505 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 87 "parser.mly"
                 ( Not )
# 511 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 88 "parser.mly"
                 ( Bool(_1) )
# 518 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 89 "parser.mly"
                 ( Equal )
# 524 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 90 "parser.mly"
                 ( Less )
# 530 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 91 "parser.mly"
                 ( Greater )
# 536 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 92 "parser.mly"
                 ( Read(_1) )
# 543 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 93 "parser.mly"
                 ( Store(_1) )
# 550 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
                 ( Drop )
# 556 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
                 ( Dropall )
# 562 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
                 ( Dup )
# 568 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 97 "parser.mly"
                 ( Swap )
# 574 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "parser.mly"
                 ( Over )
# 580 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 99 "parser.mly"
                 ( Rot )
# 586 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 100 "parser.mly"
                 ( Label(_1) )
# 593 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 101 "parser.mly"
                 ( Jump(_1) )
# 600 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 102 "parser.mly"
                 ( JumpIf(_1) )
# 607 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 103 "parser.mly"
                 ( Call(_1) )
# 614 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
                 ( Move )
# 620 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
                 ( Stop )
# 626 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser.mly"
                 ( Shoot )
# 632 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 107 "parser.mly"
                 ( Look )
# 638 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 108 "parser.mly"
                 ( IsFoe )
# 644 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 109 "parser.mly"
                 ( IsAlly )
# 650 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 110 "parser.mly"
                 ( IsWall )
# 656 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 111 "parser.mly"
                 ( IsEnd )
# 662 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 112 "parser.mly"
                 ( Wait )
# 668 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 113 "parser.mly"
                 ( GetHealth )
# 674 "parser.ml"
               : 'operation))
; (fun __caml_parser_env ->
    Obj.repr(
# 114 "parser.mly"
                 ( Random )
# 680 "parser.ml"
               : 'operation))
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
