open Drone;;
open Printf;;
open Bullet;;

class arena =
object (self)
	val mutable drones = []
	val mutable bullets = []
	val mutable debug_mode = false

	val mutable nearest_drone = new drone
	val mutable pi = 4. *. atan 1.

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


	method add_bullet dire dist =
		let b = new bullet in (
			begin
			b#set_direction dire;
			b#set_distance dist;
			end;
			bullets <- b :: bullets;
	)

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


	(* calculate distance between two drones *)
	method get_distance dire1 dist1 dire2 dist2 = 
		sqrt( ((dist1 *. cos((360. -. dire1) *. pi /. 180.)) -. (dist2 *. cos((360. -. dire2) *. pi /. 180.)))*.
			((dist1 *. cos((360. -. dire1) *. pi /. 180.)) -. (dist2 *. cos((360. -. dire2) *. pi /. 180.))) +. 
			((dist1 *. (-.sin((360. -. dire1) *. pi /. 180.))) -. (dist2 *. (-.sin((360. -. dire2) *. pi /. 180.)))) *. 
			((dist1 *. (-.sin((360. -. dire1) *. pi /. 180.))) -. (dist2 *. (-.sin((360. -. dire2) *. pi /. 180.)))) )



	 (* 	method look_enemy dire range =

			List.iter (fun d -> 
				if (d#get_direction_in_arena<=(dire-range) && d#get_direction_in_arena>=(dire-range)) && (d#get_distance_in_arena = 0.) || 

				then



			) drones 
	  *)


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
				(* TO DO ! check what the drone sees and put the result into drone's stack *)
				| Do_Look(direction)            -> ()
				(* TO DO ! create object 'bullet' with initial position the same as drone's *)				
				| Do_Shoot(direction, distance) -> self#add_bullet direction distance
													 
 			with Error_in_AI ("Main program terminated", "--", _) -> printf "%s: find call_stack is currently empty, moving on...\n" d#get_drone_name

		) drones;


		(* TO DO! For all drones and bullets: update position, call GUI if needed *)
		!live_drones

end;;
