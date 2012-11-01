module StringMap = Map.Make(String);;

type bytecode =
	  Nop
	| Int of int
	| Plus
	| Minus
	| Times
	| Divide
	| Mod
	| Power
	| And
	| Or
	| Not
	| Bool of bool
	| Equal
	| Less
	| Greater
	| Colon
	| Store of string
	| Read of string
	| Label of string
	| Drop
	| Dropall
	| Dup
	| Swap
	| Over
	| Rot
	| Jump of string
	| JumpIf of string
	| AbsJump of int
	| AbsJumpIf of int
	| Call of string
	| Move
	| Stop
	| Shoot
	| Look
	| IsFoe
	| IsAlly
	| IsWall
	| IsEnd
	| Wait
	| GetHealth
	| Random
;;

let string_of_bytecode code =
	match code with
	  Nop             -> ""											(* *)
	| Int(x)          -> "Int(" ^ (string_of_int x) ^ ")"			(* 5, integer*)
	| Plus            -> "Plus"										(* 1 2 +, addition of integers *)
	| Minus           -> "Minus"										(* 1 2 -, subtraction of integers *)
	| Times           -> "Times"										(* 1 2 *, mutip of integers *)
	| Divide          -> "Divide"									(* 1 2 /, division of integers *)
	| Mod             -> "Mod"										(* 1 2 mod, take mod of 1 by 2 *)
	| Power           -> "Power"										(* 1 2 ^, take the power of 1 by 2  *)
	| And             -> "And"                                       (* bool1 bool2 and, return bool1 && bool2 *)
	| Or              -> "Or"                                        (* bool1 bool2 or, return bool1 || bool2 *)
	| Not             -> "Not"                                       (* bool1 not, return negation of bool1  *)
	| Bool(b)         -> "Bool(" ^ (string_of_bool b) ^ ")"          (* true, boolean type true or false *)
	| Equal           -> "Equal"                                     (* 2 2 =, equal *)
	| Less            -> "Less"                                      (* 1 2 <, smaller *)
	| Greater         -> "Greater"                                   (* 2 1 >, greater *)
	| Colon           -> "Colon"                                     (* : , colon *)
	| Store(var)      -> "Store(" ^var ^ ")"                         (* 2 store , store the value of 2 *)
	| Read(var)       -> "Read(" ^var ^ ")"                          (* 2 read , read the value of 2 *)
	| Label(name)     -> "Label(" ^ name ^ ")"                       (* label1: , take the label of name label1 *)
	| Drop            -> "Drop"                                      (* a b c -> a b, drop the first element in the stack *)
	| Dropall         -> "Dropall"                                   (* a b c ->, drop all elements in the stack *)
	| Dup             -> "Dup"                                       (* a b c -> a b c c, duplicate first element in the stack *)
	| Swap            -> "Swap"                                      (* a b c -> a c b, swap the elements in the stack *)
	| Over            -> "Over"                                      (* a b c -> a b c b *)
	| Rot             -> "Rot"                                       (* a b c -> b c a *)
	| Jump(name)      -> "Jump(" ^ name ^ ")"                        (* label1 jump, jump the label names label1 *)
	| JumpIf(name)    -> "JumpIf(" ^ name ^ ")"                      (* label1 jumpif, condition jump*)
	| AbsJump(addr)   -> "AbsJump(" ^ (string_of_int addr) ^ ")"
	| AbsJumpIf(addr) -> "AbsJumpIf(" ^ (string_of_int addr) ^ ")"
	| Call(name)      -> "Call(" ^ name ^ ")"						(* call the function by the name *)
	| Move            -> "Move"
	| Stop            -> "Stop"
	| Shoot           -> "Shoot"
	| Look            -> "Look"
	| IsFoe           -> "IsFoe"
	| IsAlly          -> "IsAlly"
	| IsWall          -> "IsWall"
	| IsEnd           -> "IsEnd"
	| Wait            -> "Wait"
	| GetHealth       -> "GetHealth"
	| Random          -> "Random"
;;

type operands =
	  Undefined
	| Integer of int
	| Boolean of bool
	| Foe															(* enemy type *)
	| Ally                                                          (* friend type*)
	| Wall                                                          (* boundary of arena *)
	| End															(* end of the action *)

type sub = {                                                        (* function defined by user *)
    name   : string;                                                (* function name *)
    body   : bytecode list;                                         (* function body *)
  }

type program = bytecode list * sub list                           (* compiled, but not-linked program defintion returned from the parser *)
