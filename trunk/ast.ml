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
	| Function of string
	| Label of string
;;

let string_of_bytecode code =
	match code with
	  Nop            -> ""
	| Int(x)         -> "Int(" ^ (string_of_int x) ^ ")"
	| Plus           -> "Plus"
	| Minus          -> "Minus"
	| Times          -> "Times"
	| Divide         -> "Divide"
	| Mod            -> "Mod"
	| Power          -> "Power"
	| And            -> "And"
	| Or             -> "Or"
	| Not            -> "Not"
	| Bool(b)        -> "Bool(" ^ (string_of_bool b) ^ ")"
	| Equal          -> "Equal"
	| Less           -> "Less"
	| Greater        -> "Greater"
	| Colon          -> "Colon"
	| Store(var)     -> "Store(" ^var ^ ")"
	| Read(var)      -> "Read(" ^var ^ ")"
	| Function(name) -> "Function(" ^ name ^ ")"
	| Label(name)    -> "Label(" ^ name ^ ")"
;;

type operands =
	  Undefined
	| Integer of int
	| Boolean of bool
	| Foe
	| Ally
	| Wall
	| End

type funcion_declaration = {
    name   : string;
    body   : bytecode array;
  }
