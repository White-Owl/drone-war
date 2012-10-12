type bytecode =
	  Int of int
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
;;
