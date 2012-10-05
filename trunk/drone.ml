open Scanner;;

module StringMap = Map.Make(struct
	type t = string
	let compare x y = Pervasives.compare x y
	end)
;;

type procedure = {
	name: string;
	code: Ast.bytecode array;
	labels: StringMap;
	let compare x y = Pervasives.compare x.name y.name
}
;;

module ProcedureMap = Map.Make(procedure);;


class drone =
	object
		val mutable procedures: ProcedureMap;
		val mutable drone_name = ""
		val mutable stack: Stack;

		method get_drone_name = drone_name


(*		method print_code code =
			match code with
			  Plus        -> print_endline "Plus"
			| Minus       -> print_endline "Minus"
			| Times       -> print_endline "Times"
			| Divide      -> print_endline "Divide" *)


		method load file_name =
			drone_name <- Filename.chop_extension (Filename.basename file_name);
			let chan_in = Pervasives.open_in file_name in
			let lexbuf = Lexing.from_channel chan_in in
			let code = [] in
			try
				while true do
					code <- (Scanner.token lexbuf) :: code
				done;

				true;
			with Failure explanation -> begin
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
				true;
			end

end;;
