open Ast;;
open Parser;;

module StringMap = Map.Make(String);;

class drone =
	object (self)
		val mutable program : Ast.program = ([],[])
		val mutable drone_name="";
		val mutable stack = Stack.create;


		method get_drone_name = drone_name



		method dump_code =
			List.iter (fun x -> print_endline (string_of_bytecode x)) (List.rev (fst program));
			List.iter (fun s -> print_endline ("sub "^s.name^(string_of_int (Array.length s.body)))) (List.rev (snd program));

		method load file_name =
			drone_name <- Filename.chop_extension (Filename.basename file_name);
			let chan_in = Pervasives.open_in file_name in
			let lexbuf = Lexing.from_channel chan_in in
			program <- Parser.program Scanner.token lexbuf;

end;;
