open Ast;;
open Parser;;

class drone =
	object (self)

		(* init the containers*)
		val mutable main_body : Ast.bytecode array = [| |]
		val mutable subs = Hashtbl.create 16
		val mutable vars = Hashtbl.create 16

		val mutable ip = 0 (*global instructor pointer*)
		val mutable current_sub = ""


		(*val mutable program : Ast.program = ([],[]) *)
		(* init the filename*)
		val mutable drone_name="";
		(* init the stack *)
		val mutable stack : (Ast.operands) Stack.t= Stack.create ();

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


		(*help pop int that is convert operands to int*)
		(*but how to deal with rest types? I return 404 and print error message*)
		method pop_int=
			match (Stack.pop stack) with
			Integer op-> op
			|_ ->print_endline("error: excepted type: \"integer\"");404

		(*help pop int that is convert operands to bool*)
		(*error type return false and print error message*)
		method pop_bool=
			match (Stack.pop stack) with
			Boolean op-> op
			|_ ->print_endline("error: excepted type: \"boolean\"");false

		method pop_drop=
			match (Stack.pop stack) with
		 	_ -> ()






		method step =(*kan bu dong ba? sp indicates stack pointer, mp main pointer, subp sub pointer, vp var pointer*) 
			match (main_body.(ip)) with
		 	Int (x)  -> Stack.push (Integer x) stack  (*find integer push to stack move mp to the next*)
			(*push integer var into the stack*)
			| Plus -> let op1=self#pop_int and op2 = self#pop_int in Stack.push (Integer (op1+op2)) stack
			(*take addition of the two ints at the top of the stack, then push result into stack*)
			| Minus -> let op1=self#pop_int and op2 = self#pop_int in Stack.push (Integer (op1-op2)) stack
			(*take difference of the two ints at the top of the stack, then push result into stack*)
			| Times -> let op1 = self#pop_int and op2 = self#pop_int  in Stack.push (Integer (op1*op2)) stack
			(*take multiplcation of the two ints at the top of the stack, then push result into stack*)
			| Divide-> let op1 = self#pop_int  and op2 = self#pop_int  in Stack.push (Integer (op2/op1)) stack
			(*take division of the two ints at the top of the stack, then push result into stack*)
			| Mod -> let op1 = self#pop_int  and op2 = self#pop_int  in Stack.push (Integer (op2 mod op1)) stack
			(*take mod of the two ints at the top of the stack, then push result into stack*)
			| Power-> let op1 = self#pop_int and op2 = self#pop_int in Stack.push (Integer (int_of_float((float_of_int(op2))**(float_of_int(op1))))) stack
			(*take power of the two ints at the top of the stack, then push result into stack*)
			| Bool(x)-> if x then Stack.push (Boolean true) stack
					else Stack.push (Boolean false) stack
			(*push boolean var into the stack*)
			| And ->let op1 = self#pop_bool and op2 = self#pop_bool  in 
				if ((op1=true)&&(op2=true)) then Stack.push (Boolean true) stack
				else Stack.push (Boolean false) stack
			(*take op1&&op2 and push the boolean result into the stack*)
			| Or -> let op1 = self#pop_bool and op2 = self#pop_bool  in 
				if ((op1=true)&&(op2=false)) then Stack.push (Boolean true) stack
				else (if((op1=true)&&(op2=false)) then Stack.push (Boolean true) stack
				else (if ((op1=false)&&(op2=true))	then Stack.push (Boolean true) stack
				else Stack.push (Boolean false) stack))
			(*take op1||op2 and push the boolean result into the stack*)
			| Not -> let op = self#pop_bool in
				if op=true then Stack.push (Boolean false) stack
				else if op=false then Stack.push (Boolean true) stack
				else print_endline("error: excepted boolean ")
			(*take !op and push the boolean result into the stack*)
			| Less -> let op1 = self#pop_bool and op2 = self#pop_bool  in 
				if op2 < op1 then Stack.push (Boolean true) stack
				else Stack.push (Boolean false) stack
			(*check if op1 is less than op2 return boolean type into the stack*)
			| Greater -> let op1 = self#pop_bool and op2 = self#pop_bool  in 
				if op2 > op1 then Stack.push (Boolean true) stack
				else Stack.push (Boolean false) stack
			(*check if op1 is greater than op2 return boolean type into the stack*)

			


			| Store(varName) -> let op = self#pop_int in Hashtbl.add vars varName op
			(*store value in stack in var and pop it*)
			| Read(varName) -> let op = (Integer(Hashtbl.find vars varName)) in Stack.push op stack
			(*push value of var to stack*)
			| Drop ->self#pop_drop
			(*pop the top var on the stack*)
			| Dropall->Stack.clear stack
			(*pop all vars on the stack*)
			| Dup->let op=Stack.top stack in Stack.push op stack
			(*copy the var on the top of the stack*)

			|_-> ()







		(*print top of stack to help check if code is right*)
		method prt =
			match  (Stack.pop stack) with
			Integer op ->print_endline("*********"); print_endline("Top of stack now is:"^string_of_int(op))
			|Boolean op->print_endline("*********");print_endline("Top of stack now is:"^string_of_bool(op))
			| _ -> print_endline("*********");print_endline("nothing to print")
		 

end;;
