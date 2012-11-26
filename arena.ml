open Drone;;
open Printf;;

class arena =
object (self)
	val mutable drones = []
	val mutable bullets = []
	val mutable debug_mode = false

	method set_debug_mode mode =
		debug_mode <- mode

	method load file_name =
		let d = new drone in (
			d#load file_name;
			if debug_mode then begin
				let decompiled_file = open_out (file_name ^ ".decompiled") in
				d#decompile decompiled_file;
				close_out decompiled_file;

				d#set_debug_output (open_out (file_name ^ ".debug"))
			end;
			drones <- d :: drones;
		)

	method get_drone_count = List.length drones;


	method run =
		let steps = ref 1 in
		while (self#step > 1) && (!steps < 100) do
			incr steps
		done;
		printf "Results:\n";
		List.iter (fun d ->
			printf "%s: %s\n" d#get_drone_name
				(if d#is_brain_dead then
					("brain dead after " ^ (string_of_int d#get_ai_ticks))
				 else if not d#is_alive then
					("died after " ^ (string_of_int d#get_ai_ticks))
				 else
					("still alive with " ^ (string_of_int d#get_health) ^ "% of health ")
				)
		) drones



	method step =
		let live_drones = ref 0 in 		(* to check how many drones are still alive and kicking *)
		List.iter (fun d ->
			if (d#is_alive) && (not d#is_brain_dead) then
				incr live_drones;

			(* TO DO! d#step can terminate with Error_in_AI exception. Catch it and deal with it gracefully *)
			try
				let action = d#step in  
				match action with
				  No_Action                     -> ()
				| Do_Look(direction)            -> ( (* TO DO! *) ) (* check what the drone sees and put the result into drone's stack *)
				| Do_Shoot(direction, distance) -> ( (* TO DO! *) ) (* create object 'bullet' with initial position the same as drone's *)
 			with Error_in_AI ("Main program terminated", "--", _) -> print_endline (d#get_drone_name ^ ": find stack is empty, moving on...")

		) drones;
		(* TO DO! For all drones and bullets: update position, call GUI if needed *)
		!live_drones

end;;
