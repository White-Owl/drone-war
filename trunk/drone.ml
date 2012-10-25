open Ast;;
open Parser;;
module StringMap = Map.Make(String);;

class drone =
	object (self)

		(* init the containers*)
		val mutable program : Ast.program = ([],[])
		(* init the filename*)
		val mutable drone_name="";
		(* init the stack *)
		val mutable stack = Stack.create;

		(* function to get the filename *)
		method get_drone_name = drone_name

		(* print out all operations and user functions in the containers *)
		method dump_code =
			(* operation container *)
			List.iter (fun x -> print_string ((string_of_bytecode x) ^ " ")) (List.rev (fst program));

			print_endline("");

			(* user function container *)
			List.iter (fun s -> print_endline ("sub " ^ s.name^(string_of_int (Array.length s.body)))) (List.rev (snd program));

		(* function to get filename *)
		method load file_name =
			drone_name <- Filename.chop_extension (Filename.basename file_name);
			let chan_in = Pervasives.open_in file_name in
			let lexbuf = Lexing.from_channel chan_in in
			program <- Parser.program Scanner.token lexbuf;

end;;
