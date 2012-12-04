open Ast;;
open Parser;;
open Parser_dbt;;
open Printf;;
open Utils;;

exception Error_in_AI of string * string * int;;

type drone_action =
	  No_Action
	| Do_Shoot of int * int
	| Do_Look of int


class drone =
	object (self)

		(* init the containers*)
		val mutable subs = Hashtbl.create 16
		val mutable vars : (string, Ast.operands) Hashtbl.t = Hashtbl.create 16
		val mutable current_sub = "--"
		val mutable instruction_pointer = 0
		val mutable call_stack: (string * int) Stack.t = Stack.create ()
		val mutable stack : (Ast.operands) Stack.t = Stack.create ()

		(* variables to enable debug functionality *)
		val mutable debug_mode = false
		val mutable debug_out_file = stderr (* channel for debug output *)
		val mutable tick_counter = 0        (* life-time ticks counter, used in debug output function *)

		(* various members *)
		val mutable drone_name = ""  (* name of the drone for GUI *)
		val mutable team_id    = 0   (* id of the team this drone belongs to *)

		(* variuables to describe current drone state *)
		val mutable health = 100
		val mutable direction_of_the_body = 0 (* used by GUI to draw where the drone is moving if drone's image is not a circle *)
		val mutable direction_of_the_gun = 0  (* used by GUI to draw where the drone's gun is pointing (direction of the last SHOOT command *)
		val mutable ticks_to_wait = 0         (* if non-zero, the AI will skip a step *)
		val mutable moving = false            (* does the drone moving or not? *)
		val mutable brain_dead = false        (* will become true if at some step the drone caught an exception *)
		val mutable reason_for_coma = ""      (* explanation why AI died *)

		val mutable x_position = 0.	  (* used by other drones to determine the position in the arena can set maximum in Arena as Radius of the circle*)
		val mutable y_position = 0.	  (* used by other drones to determine the position in the arena 0-360*)

		(* maxmium bullet load is 5 can be displayed in the GUI *)
		val mutable bullet_capacity = 5
		val mutable has_bullet = true
		(* set to 10 each time drone shoots. drone cannot shoot until gun_cooldown returns to zero *)
		val mutable gun_cooldown = 0

		method get_moving_direction = direction_of_the_body

		method set_moving_direction dire = direction_of_the_body <- dire

		method get_x_position = x_position

		method set_x_position x = x_position <- x

		method get_y_position = y_position

		method set_y_position y = y_position <- y

		method get_current_sub = current_sub;

		method get_drone_name = drone_name

		method is_brain_dead = brain_dead

		method is_alive = (health > 0)

		method get_ai_ticks = tick_counter

		method get_health = health

		method belongs_to_team id = team_id <- id

		method get_team_id = team_id

		method get_moving_status = moving

		method set_health h = health <- max h 0


		(* this method is called, by the engine's LOOK procedure *)
		method found_target dist dire flag=
			if flag<>End then
			begin
				Stack.push (Integer (dist)) stack;
				Stack.push (Integer (dire)) stack;
			end;
			Stack.push (Flag (flag)) stack


		method move speed =
			if moving then
			begin
				y_position <- y_position +. (float_of_int(speed) *. (cos (float_of_int(direction_of_the_body) *. pi /. 180.)));
				x_position <- x_position +. (float_of_int(speed) *. (sin (float_of_int(direction_of_the_body) *. pi /. 180.)));

				(* check did we hit a wall? *)
				if x_position > 1000. || x_position < 0. || y_position > 1000. || y_position < 0. then
				begin
					self#set_health (health - 10);
					if x_position > 1000. then x_position <- 1000.;
					if x_position < 0. then x_position <- 0.;
					if y_position > 1000. then y_position <- 1000.;
					if y_position < 0. then y_position <- 0.;
					(* this is still debated, what to do after hiting the wall, stop or bounce from it? *)
					(* direction_of_the_body <- ((direction_of_the_body + 180) mod 360; (* bouncing adds more chaos to the battle *) *)
					moving <- false;   (* stopping is more easy to predict and explain *)
					if health=0 then moving <- false (* if drone died after hitting the wall, it definetely will not move anymore *)
				end
			end


		method set_debug_output out_file =
			debug_out_file <- out_file;
			debug_mode <- true

		(* print out all operations in the container *)
		method dump_code body_as_array out_file =
			let command_counter = ref 0 in
			Array.iter (fun x ->
							fprintf out_file "%3d: %s\n" !command_counter (string_of_bytecode x);
							command_counter := !command_counter +1
						) body_as_array

		(* decompile the program into compilable text *)
		method decompile out_file =
			let body = (Hashtbl.find subs "--") in self#dump_code body out_file;
			Hashtbl.iter (fun name body ->
			              if not (name="--") then begin
				              fprintf out_file "\nsub %s\n" name;
				              self#dump_code body out_file;
				              fprintf out_file "esub\n"
			              end
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
			                           Jump(name) ->
			                               if not (Hashtbl.mem lbls name) then raise (Failure ("Label "^name^" is not defined"));
			                               AbsJump( Hashtbl.find lbls name )
			                         | JumpIf(name) ->
			                               if not (Hashtbl.mem lbls name) then raise (Failure ("Label "^name^" is not defined"));
			                               AbsJumpIf( Hashtbl.find lbls name )
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
			let program =
				(if (Filename.check_suffix file_name ".dt" ) then Parser.program Scanner.token lexbuf
				 else if (Filename.check_suffix file_name ".dbt" ) then Parser_dbt.program Scanner_dbt.drone_basic lexbuf
				 else ([],[])
				) in
			(* Parser will return two lists - list operations of main program and list of subs
			   First we need to put "main" body of the program into the list of subs *)
			Hashtbl.add subs "--" (self#link_jumps (List.rev (fst program)));
			(* Second, we add all subs recognized by parser into the list of subs, as a side effect - check for duplicate subs *)
			List.iter (fun sub ->
			            if Hashtbl.mem subs sub.name then raise (Failure ("Sub "^sub.name^" defined twice"));
						Hashtbl.add subs sub.name (self#link_jumps (List.rev sub.body))
			          ) (snd program);
			(* Third step, check the existance of all called user funcitons *)
			Hashtbl.iter (fun name body -> (self#check_sub_existance body)) subs;
			(* Last step, set starting position for the drone *)
			self#set_x_position (Random.float 1000.);
			self#set_y_position (Random.float 1000.);
			self#set_moving_direction (Random.int 360)
			(* self#print_current_pos; *)


		(* helping pop function which converts operand to integer *)
		method pop_int=
			if Stack.is_empty stack then self#freeze "Empty stack";
			match (Stack.pop stack) with
			Integer op-> op
			| _ -> self#freeze "Type mismatch"; 0

		(* helping pop function which converts operand to bool *)
		method pop_bool=
			if Stack.is_empty stack then self#freeze "Empty stack";
			match (Stack.pop stack) with
			Boolean op -> op
			| _ -> self#freeze "Type mismatch"; false

		(* helping pop function which converts operand to look_flag *)
		method pop_flag=
			if Stack.is_empty stack then self#freeze "Empty stack";
			match (Stack.pop stack) with
			Flag op -> op
			| _ -> self#freeze "Type mismatch"; End



		method step =
			tick_counter <- tick_counter+1;
			if gun_cooldown>0 then gun_cooldown <- gun_cooldown-1;
    		if ticks_to_wait > 0 then begin
				if debug_mode then begin
					fprintf debug_out_file "%4d  waiting for %d ticks\n" tick_counter ticks_to_wait;
				end;
				ticks_to_wait <- ticks_to_wait-1;
				No_Action
			end else begin
				let body = (Hashtbl.find subs current_sub) in
				if (Array.length body) = instruction_pointer then begin
					if Stack.is_empty call_stack then self#freeze "Main program terminated";
					let return_address = (Stack.pop call_stack) in begin
						current_sub <- fst return_address;
						instruction_pointer <- snd return_address;
					end;
					No_Action
				end else begin
					if debug_mode then self#print_current_state;
    				let action = match Array.get body instruction_pointer with
						(* primitive types *)
					 	  Int (x) -> Stack.push (Integer x) stack; No_Action
						| Bool(x) -> Stack.push (Boolean x) stack; No_Action

						(* simple arithmetics *)
						| Plus   -> let op2=self#pop_int and op1=self#pop_int in Stack.push (Integer (op1 + op2)) stack; No_Action
						| Minus  -> let op2=self#pop_int and op1=self#pop_int in Stack.push (Integer (op1 - op2)) stack; No_Action
    					| Times  -> let op2=self#pop_int and op1=self#pop_int in Stack.push (Integer (op1 * op2)) stack; No_Action
						| Divide -> let op2=self#pop_int and op1=self#pop_int in Stack.push (Integer (op1 / op2)) stack; No_Action
						| Mod    -> let op2=self#pop_int and op1=self#pop_int in Stack.push (Integer (op1 mod op2)) stack; No_Action
						| Power  -> let op2=self#pop_int and op1=self#pop_int in Stack.push (Integer (int_of_float((float_of_int(op1)) ** (float_of_int(op2))))) stack; No_Action

    					(* boolean arithmetics *)
						| And -> let op2=self#pop_bool and op1=self#pop_bool in Stack.push (Boolean (op1 && op2)) stack; No_Action
						| Or  -> let op2=self#pop_bool and op1=self#pop_bool in Stack.push (Boolean (op1 || op2)) stack; No_Action
						| Not -> let op=self#pop_bool in Stack.push (Boolean (not op)) stack; No_Action

    					(* conditions *)
						| Less    -> let op2=self#pop_int and op1=self#pop_int in Stack.push (Boolean (op1 < op2)) stack; No_Action
						| Greater -> let op2=self#pop_int and op1=self#pop_int in Stack.push (Boolean (op1 > op2)) stack; No_Action
						| Equal   -> let op2=self#pop_int and op1=self#pop_int in Stack.push (Boolean (op1 = op2)) stack; No_Action

						(* call anothe sub*)
						| Call(name) -> begin
										Stack.push (current_sub, (instruction_pointer+1)) call_stack;
										current_sub <- name;
										instruction_pointer <- -1
										end;
					                    No_Action
    					(* variables *)
						| Store(varName) -> if Stack.is_empty stack then self#freeze "Nothing to store";
											let op = Stack.pop stack in Hashtbl.replace vars varName op;
    										No_Action

						| Read(varName)  -> if not (Hashtbl.mem vars varName) then self#freeze "Variable not defined";
											let op = Hashtbl.find vars varName in
											Stack.push op stack;
											No_Action

						(* stack manipulation *)
						| Drop    -> ignore(Stack.pop stack); No_Action
						| Dropall -> Stack.clear stack; No_Action
    					| Dup     -> let op=Stack.top stack in Stack.push op stack; No_Action
						| Swap    -> let op2=Stack.pop stack and op1=Stack.pop stack in begin Stack.push op1 stack; Stack.push op2 stack end; No_Action
						| Over    -> let op2=Stack.pop stack and op1=Stack.top stack in begin Stack.push op1 stack; Stack.push op2 stack end; No_Action
						| Rot     -> let op3=Stack.pop stack and op2=Stack.top stack and op1=Stack.top stack in begin Stack.push op2 stack; Stack.push op3 stack; Stack.push op1 stack end; No_Action

						(* game specific operations *)
						| Move      -> let direction=self#pop_int in direction_of_the_body <- direction; moving <- true; No_Action
						| Stop      -> moving <- false; No_Action
						| Shoot     -> let direction=self#pop_int and distance=self#pop_int in
										direction_of_the_gun <- direction;
										Stack.push (Boolean (gun_cooldown=0)) stack;
										if gun_cooldown>0
											then No_Action
											else (gun_cooldown<-10; Do_Shoot(direction, distance))

						| Look      -> let direction=self#pop_int in Do_Look(direction)
						| IsFoe     -> let flag=self#pop_flag in Stack.push (Boolean (flag=Foe)) stack; No_Action
						| IsAlly    -> let flag=self#pop_flag in Stack.push (Boolean (flag=Ally)) stack; No_Action
						| IsWall    -> let flag=self#pop_flag in Stack.push (Boolean (flag=Wall)) stack; No_Action
						| IsEnd     -> let flag=self#pop_flag in Stack.push (Boolean (flag=End)) stack; No_Action
						| GetHealth -> Stack.push (Integer(health)) stack; No_Action
						| Wait      -> ticks_to_wait <- self#pop_int; No_Action

						(* TO DO! get random int between min and max *)
						| Random    -> let max=self#pop_int and min=self#pop_int in Stack.push (Integer(Random.int (max - min + 1) + min)) stack; No_Action

						(* jumps *)
						| AbsJump(x)   -> instruction_pointer <- x-1; No_Action
						| AbsJumpIf(x) -> if self#pop_bool then instruction_pointer <- x-1; No_Action
						| _ -> No_Action
					in
					instruction_pointer <- instruction_pointer+1;
					action
				end
			end

		method print_current_pos =
			begin
				print_endline drone_name;
				print_float x_position;
				print_endline "";
				print_float y_position;
				print_endline "";
				print_endline "Direction: ";
				print_int direction_of_the_body;
				print_endline "";
				print_endline "Health: ";
				print_int health;
				print_endline "";
				print_endline "Bullet load: ";
				print_int bullet_capacity;
				print_endline "";
				print_endline "";
			end


		method freeze explanation =
			brain_dead <- true;
			reason_for_coma <- explanation;
			raise (Error_in_AI (explanation, current_sub, instruction_pointer));

		method get_reason_for_coma = reason_for_coma

		method print_current_state =
			let sub_name = (if current_sub="--" then "" else current_sub) in
			let body = (Hashtbl.find subs current_sub) in
			let bc = Array.get body instruction_pointer in
			fprintf debug_out_file "%4d %20s[%3d] %20s  |" tick_counter sub_name instruction_pointer (string_of_bytecode bc);
			let stack_copy = Stack.copy stack in
			let cnt = ref 1 in
			while (!cnt < 10) && (not (Stack.is_empty stack_copy)) do
				let op = Stack.pop stack_copy in
				fprintf debug_out_file " %s" (string_of_operand op);
				cnt := !cnt +1
			done;
			if (Stack.is_empty stack_copy) then
				fprintf debug_out_file " EOS\n"
			else
				fprintf debug_out_file " ...\n"


		(* for each shoot update bullet capacity and push boolean on the stack *)
		method update_bullet_load =
			begin
			(* shoot *)
			if bullet_capacity > 0
			then
				begin
					bullet_capacity <- bullet_capacity - 1;
					has_bullet <- true;
				end
			(* no bullet *)
			else
				has_bullet <- false;
			end;
			has_bullet

end;;
