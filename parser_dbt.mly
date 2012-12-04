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


let report_error error_starts_at message =
    raise (Parse_failure (message, error_starts_at.pos_lnum, (error_starts_at.pos_cnum-error_starts_at.pos_bol+1)))
	;;

%}

%token CR
%token IF THEN ELSE
%token DO LOOP WHILE UNTIL EXIT
%token SUB
%token END
%token FOR TO STEP NEXT
%token GOTO
%token <bool> BOOL
%token <string> ID
%token <int> INT
%token LPAREN RPAREN COLON COMMA
%token PLUS MINUS TIMES DIVIDE
%token EQUAL NOT_EQUAL
%token LESS GREATER LESS_EQUAL GREATER_EQUAL
%token AND OR NOT
%token EOF

%left AND OR NOT
%left EQUAL NOT_EQUAL
%left LESS GREATER LESS_EQUAL GREATER_EQUAL
%left PLUS MINUS
%left TIMES DIVIDE


%start program
%type <Ast.program> program

%%

program: {[],[]} /* at the begining we have nothing */
  | program statement            { ($2 @ fst $1), snd $1 }
  | program compaund_statement   { ($2 @ fst $1), snd $1 }
  | program sub                  { fst $1, ($2 :: snd $1) }        /* add user function to the list of subs */


statements:
	  /* nothing */                  { [] }
	| statements CR                  { $1 }
	| statements statement           { $2 @ $1 }
	| statements compaund_statement  { $2 @ $1 }


statement:
    ID EQUAL math_expr CR     { Store($1) :: $3 }
  | EXIT DO CR                { [ Jump("--ExitDo") ] }
  | EXIT FOR CR               { [ Jump("--ExitFor") ] }
  | GOTO ID CR                { [ Jump($2) ] }
  | ID COLON                  { [ Label($1) ] }
  | error CR                  { report_error (Parsing.rhs_start_pos 1) "Syntax error" }


compaund_statement:
    IF condition THEN statement
		{ let lbl = make_label() in
		  Label(lbl) :: ( $4 @ ( [ JumpIf(lbl) ; Not ]  @ $2 ) )
		}
  | IF condition THEN CR statements END IF
		{ let lbl = make_label() in
		  Label(lbl) :: ( $5 @ ( [ JumpIf(lbl) ; Not ]  @ $2 ) )
		}
  | IF condition THEN CR statements ELSE CR statements END IF
		{ let lblTrue = make_label() in
		  let lblEndIf = make_label() in
		  Label(lblEndIf) :: ( $5 @ (Label(lblTrue) :: Jump(lblEndIf) :: ( $8 @ ( JumpIf(lblTrue) :: $2) ) ) )
		}
  | DO WHILE condition CR statements LOOP
		{ let lblStart = make_label() and lblCheck = make_label() and lblDone = make_label() in
		  let block = List.map (fun x -> match x with Jump("--ExitDo") -> Jump(lblDone) | _ -> x) $5 in
		  Label(lblDone) :: JumpIf(lblStart) :: (block @ ([ Label(lblCheck) ] @ ($3 @ [Label(lblStart); Jump(lblCheck)])))
		}
  | DO statements LOOP WHILE condition
		{ let lblStart = make_label() and lblDone = make_label() in
		  let block = List.map (fun x -> match x with Jump("--ExitDo") -> Jump(lblDone) | _ -> x) $2 in
		  Label(lblDone) :: JumpIf(lblStart) :: ($5 @ (block @ [Label(lblStart)]))
		}
  | DO UNTIL condition CR statements LOOP
		{ let lblStart = make_label() and lblCheck = make_label() and lblDone = make_label() in
		  let block = List.map (fun x -> match x with Jump("--ExitDo") -> Jump(lblDone) | _ -> x) $5 in
		  Label(lblDone) :: JumpIf(lblStart) :: Not :: ($3 @ ([ Label(lblCheck) ] @ (block @ [Label(lblStart); Jump(lblCheck)])))
		}
  | DO statements LOOP UNTIL condition
		{ let lblStart = make_label() and lblDone = make_label() in
		  let block = List.map (fun x -> match x with Jump("--ExitDo") -> Jump(lblDone) | _ -> x) $2 in
		  Label(lblDone) :: JumpIf(lblStart) :: Not :: ($5 @ (block @ [Label(lblStart)]))
		}
  | FOR ID EQUAL math_expr TO math_expr CR statements NEXT
		{ let lblAgain = make_label() and lblDone = make_label() in
		  let block = List.map (fun x -> match x with Jump("--ExitFor") -> Jump(lblDone) | _ -> x) $8 in
		  [Label(lblDone); JumpIf(lblAgain); Less] @ $6 @ [ Store($2); Dup; Plus; Int(1); Read($2)] @ block @ [Label(lblAgain); Store($2)] @ $4
		}
  | FOR ID EQUAL math_expr TO math_expr STEP math_expr CR statements NEXT
		{ let lblAgain = make_label() and lblDone = make_label() in
		  let block = List.map (fun x -> match x with Jump("--ExitFor") -> Jump(lblDone) | _ -> x) $10 in
		  [Label(lblDone); JumpIf(lblAgain); Less] @ $6 @ [ Store($2); Dup; Plus] @ $8 @ [Read($2)] @ block @ [Label(lblAgain); Store($2)] @ $4
		}

sub:
    SUB ID LPAREN args RPAREN CR statements END SUB CR
		{ let sub_body = List.map(fun x -> match x with
		                            Read(name) -> if List.exists (fun arg -> arg=name) $4 then Read($2^"-"^name) else Read(name)
		                          | Store(name) -> if List.exists (fun arg -> arg=name) $4 then Store($2^"-"^name) else Store(name)
		                          | _ -> x) $7 in
		  { name = $2; body = sub_body; }
		}


args: { [] }
  | ID            { [ $1 ] }
  | args COMMA ID { $3 :: $1 }


condition:
    logic_expr                     { $1 }
  | logic_expr AND logic_expr      { And :: ($3 @ $1) }
  | logic_expr OR logic_expr       { Or :: ($3 @ $1) }
  | NOT logic_expr                 { Not :: $2 }
  | error                          { report_error (Parsing.rhs_start_pos 1) "Malformed logical expression" }


logic_expr:
    BOOL                               { [ Bool($1) ] }
  | LPAREN logic_expr RPAREN           { $2 }
  | math_expr math_relation math_expr  { $2 @ ( $3 @ $1) }


math_relation:
    EQUAL         { [ Equal ] }
  | NOT_EQUAL     { [ Equal ; Not ] }
  | LESS          { [ Less ] }
  | GREATER       { [ Greater ] }
  | LESS_EQUAL    { [ Greater ; Not ] }
  | GREATER_EQUAL { [ Less ; Not ] }


math_expr:
    INT                          { [ Int($1) ] }
  | ID                           { [ Read($1) ] }
  | math_expr PLUS math_expr     { Plus :: ( $3 @ $1) }
  | math_expr MINUS math_expr    { Minus :: ( $3 @ $1) }
  | math_expr TIMES math_expr    { Times :: ( $3 @ $1) }
  | math_expr DIVIDE math_expr   { Divide :: ( $3 @ $1) }
  | LPAREN math_expr RPAREN      { $2 }
  | error                          { report_error (Parsing.rhs_start_pos 1) "Malformed math expression" }
