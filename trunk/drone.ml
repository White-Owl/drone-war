open Ast;;
open Parser;;
module StringMap = Map.Make(String);;

class drone =
	object (self)

		(* init the containers*)
		val mutable main_body : Ast.bytecode array = [| |]
		val mutable subs = StringMap.empty
		val mutable vars = StringMap.empty

		(*val mutable program : Ast.program = ([],[]) *)
		(* init the filename*)
		val mutable drone_name="";
		(* init the stack *)
		val mutable stack = Stack.create;

		(* function to get the filename *)
		method get_drone_name = drone_name

		(* print out all operations and user functions in the containers *)
		method dump_code =
			(* operation container *)
			List.iter (fun x -> print_string ((string_of_bytecode x) ^ " ")) main_body;
			print_newline();

			(* user function container *)
			(*List.iter (fun s -> print_endline ("sub " ^ s.name ^ " " ^(string_of_int (Array.length s.body)))) (List.rev (snd program));*)


		method linker bclist =
			let lbls = Hashtbl.empty in
			let no_label = List.fold (fun acc x ->
			                          match x with
			                            Label(name) ->
			                               if Hashtbl.mem lbls name then raise (Failure ("Label "^name^" defined twice"))
			                               else Hashtbl.add lbls name (List.length acc);
			                               acc
			                          | _ -> x::acc
			                         ) [] bclist in
			let abs_jumps = List.map(fun x -> match x with
			                           Jump(name) -> AbsJump( Hashtbl.find lbls name )
			                         | JumpIf(name) -> AbsJumpIf( Hashtbl.find lbls name )
			                         | _ -> _ ) no_label in
			Array.of_list (List.rev abs_jumps)


		(* function to get filename *)
		method load file_name =
			drone_name <- Filename.chop_extension (Filename.basename file_name);
			let chan_in = Pervasives.open_in file_name in
			let lexbuf = Lexing.from_channel chan_in in
			let program = Parser.program Scanner.token lexbuf in
			main_body <- self#linker (fst program)

end;;
