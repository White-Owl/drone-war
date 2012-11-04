open Ast;;
open Parser;;

class drone =
	object (self)

		(* init the containers*)
		val mutable main_body : Ast.bytecode array = [| |]
		val mutable subs = Hashtbl.create 16
		val mutable vars = Hashtbl.create 16

		(*val mutable program : Ast.program = ([],[]) *)
		(* init the filename*)
		val mutable drone_name="";
		(* init the stack *)
		val mutable stack = Stack.create();

		(* function to get the filename *)
		method get_drone_name = drone_name

		(* print out all operations in the container *)
		method dump_code body_as_array =
			Array.iter (fun x -> print_string ((string_of_bytecode x) ^ " ")) body_as_array

		(* decompile the program into compilable text *)
		method decompile = 
			self#dump_code main_body;
			print_newline();
			Hashtbl.iter (fun name body ->
			              print_string ("sub "^name^" ");
			              self#dump_code body;
			              print_endline ("esub");
			             ) subs

		(* takes a raw list of operators including a Label(name) operator,
		   Remove all label, put them into temporary hash table
		   Using this hash table satisfy all jump(name) and convert them to jump(address) *)
		method link_jumps body_as_list =
			let lbls = Hashtbl.create 16 in
			let no_label = List.fold_left (fun acc x ->
			                          match x with
			                            Label(name) ->
			                               if Hashtbl.mem lbls name then raise (Failure ("Label "^name^" defined twice"))
			                               else Hashtbl.add lbls name (List.length acc);
			                               acc
			                          | _ -> x::acc
			                         ) [] body_as_list in
			let abs_jumps = List.map(fun x -> match x with
			                           Jump(name) -> AbsJump( Hashtbl.find lbls name )
			                         | JumpIf(name) -> AbsJumpIf( Hashtbl.find lbls name )
			                         | _ -> x ) no_label in
			Array.of_list (List.rev abs_jumps)

		(* check existance of a called sub, complain if it is not defined *)
		method check_sub_existance body =
			Array.iter (fun x -> match x with
			              Call(name) -> if not (Hashtbl.mem subs name) then raise (Failure ("Sub "^name^" is not defined"))
			             | _ -> ()
			           ) body


		(* Read the drone *)
		method load file_name =
			drone_name <- Filename.chop_extension (Filename.basename file_name);
			let chan_in = Pervasives.open_in file_name in
			let lexbuf = Lexing.from_channel chan_in in
			let program = Parser.program Scanner.token lexbuf in
			(* parser will return two lists - list operations of main program and list of subs *)
			(* first, we need to convert all jumps to label into jumps to absolute position in the code,
			   as a side-effect it will ensure that all jumps are legal *)
			main_body <- self#link_jumps (List.rev (fst program));
			List.iter (fun sub ->
			            if Hashtbl.mem subs sub.name then raise (Failure ("Sub "^sub.name^" defined twice"))
						else Hashtbl.add subs sub.name (self#link_jumps sub.body) 
			          ) (snd program);
			(* second step, check the existance of all called user funcitons *)
			self#check_sub_existance main_body;
			Hashtbl.iter (fun name body -> (self#check_sub_existance body)) subs



		method step = let rec execute_byte mp =(*kan bu dong ba? sp indicates stack pointer, mp main pointer, subp sub pointer, vp var pointer*) 
		match (main_body.(mp)) with
		 Int (x)  -> Stack.push x stack; execute_byte (mp+1)  (*find integer push to stack move mp to the next*)
		| Plus -> let op1 = Stack.pop stack and op2 =Stack.pop stack  in Stack.push (op1+op2) stack ; execute_byte (mp+1)
		| Minus -> let op1 = Stack.pop stack and op2 =Stack.pop stack  in Stack.push (op1-op2) stack ; execute_byte (mp+1)
		| Times -> let op1 = Stack.pop stack and op2 =Stack.pop stack  in Stack.push (op1*op2) stack ; execute_byte (mp+1)
		| Divide-> let op1 = Stack.pop stack and op2 =Stack.pop stack  in Stack.push (op2/op1) stack ; execute_byte (mp+1)
		| Mod -> let op1 = Stack.pop stack and op2 =Stack.pop stack  in Stack.push (op2 mod op1) stack ; execute_byte (mp+1)
		| Bool(x)-> if x then Stack.push 1 stack
					else Stack.push 0 stack ; execute_byte (mp+1)
		| Power-> let op1 = Stack.pop stack and op2=Stack.pop stack in Stack.push (int_of_float((float_of_int(op2))**(float_of_int(op1)))) stack ; execute_byte (mp+1)
		| And ->let op1 = Stack.pop stack and op2 =Stack.pop stack  in 
				if ((op1=1)&&(op2=1)) then Stack.push 1 stack
				else Stack.push 0 stack; 
		 		execute_byte (mp+1)
		| Or -> let op1 = Stack.pop stack and op2 =Stack.pop stack  in 
				if ((op1=1)&&(op2=1)) then Stack.push 1 stack
				else (if((op1=1)&&(op2=0)) then Stack.push 1 stack
				else (if ((op1=0)&&(op2=1))	then Stack.push 1 stack
				else Stack.push 0 stack)); 
			execute_byte (mp+1)
		| Not -> let op= Stack.pop stack in
				if op=1 then Stack.push 0 stack
			else if op=0 then Stack.push 1 stack
		else print_endline("not boolean in the top of stack");
		execute_byte(mp+1)


		|_-> ()
		in execute_byte 0 


		method prt = print_endline (string_of_int(Stack.pop stack)) (*print top of the stack*)











end;;
