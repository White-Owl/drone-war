type bytecode =
	  Int of int
	| Plus
	| Minus
	| Times
	| Divide


type expr =
	  Operator of expr
	| Integer of int
