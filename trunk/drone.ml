open Ast;;
open Scanner;;

module StringMap = Map.Make(String);;


class drone =
	object (self)
		val mutable procedures = StringMap.empty;
		val mutable drone_name="";
		val mutable stack = Stack.create;


		method get_drone_name = drone_name


		method string_of_bytecode code =
			match code with
			  Int(x)         -> "Int(" ^ (string_of_int x) ^ ")"
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


		method dump_code =
			StringMap.iter (fun procedure_name procedure_code ->
				print_string ("Procedure " ^ procedure_name ^ ":");
				List.iter (fun code ->
					print_char ' ';
					print_string (self#string_of_bytecode code)
				) procedure_code;
				print_newline();
			) procedures


		method load file_name =
			drone_name <- Filename.chop_extension (Filename.basename file_name);
			let chan_in = Pervasives.open_in file_name in
			let lexbuf = Lexing.from_channel chan_in in
			let current_function = ref " " in
			let code = ref [] in
			try
				while true do
					code := (Scanner.token lexbuf) :: !code;
				done;

				true;
			with Unrecognized_Token token -> begin
				let pos = Lexing.lexeme_start_p lexbuf in
				print_string ("\nReading drone " ^ drone_name ^ " failed \nUnrecognized token '" ^ token ^ "' at ");
				print_int pos.Lexing.pos_lnum;
				print_char ':';
				print_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol +1);
				print_newline();
				Pervasives.close_in chan_in;
				false;
			end
			| Failure explanation -> begin
				let pos = Lexing.lexeme_start_p lexbuf in
				print_string ("\nReading drone " ^ drone_name ^ " failed \n" ^ explanation ^ " at ");
				print_int pos.Lexing.pos_lnum;
				print_char ':';
				print_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol +1);
				print_newline();
				Pervasives.close_in chan_in;
				false;
			end
			| End_of_file -> begin
				Pervasives.close_in chan_in;
				procedures <- StringMap.add !current_function (List.rev !code) procedures;
				true;
			end

end;;
