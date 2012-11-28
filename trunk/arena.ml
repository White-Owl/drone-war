open Drone;;
open Printf;;
open Bullet;;

class arena =
object (self)
	val mutable drones = []
	val mutable bullets = []
	val mutable debug_mode = false

	val mutable pi = 4. *. atan 1.
	val mutable look_range = 30 		(*+30 and -30 on the given degree*)
	val mutable bullet_speed = 500	
	val mutable drone_speed = 100

	val mutable area_map_x = 1000
	val mutable area_map_y = 1000


	method set_debug_mode mode =
		debug_mode <- mode

	method load file_name =
		let d = new drone in (
			d#load file_name;
			if debug_mode then begin
				let decompiled_file = open_out (file_name ^ ".decompiled") in
				d#decompile decompiled_file;
				close_out decompiled_file;

				(*set random of drone's position*)
				d#set_x_position (Random.int 1000);
				d#set_y_position (Random.int 1000);

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


	method get_distance x1 y1 x2 y2 = 
		int_of_float(sqrt(float_of_int((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2))))


  	(* method look_one_drone dire d_shoot d_target = 
		let x_target_pos = d_target#get_x_position and 
			y_target_pos = d_target#get_y_position and
			x_shoot_pos = d_shoot#get_x_position and
			y_shoot_pos = d_shoot#get_y_position in
			let target_dire = int_of_float(atan( float_of_int(y_target_pos - y_shoot_pos) /. float_of_int(x_target_pos - x_shoot_pos)) *. 180. /. pi) and
				distance = self#get_distance x_target_pos y_target_pos x_shoot_pos y_shoot_pos	
			in 
			if target_dire < (dire + look_range) && target_dire > (dire - look_range)
			then 
				d_shoot#add_found_target distance target_dire

	 *)			

	method update_drone_position =
		List.iter (fun d ->
			begin
			d#move;
			if d#check_hit_wall = true
			then
				begin
				d#update_hit_pos;
				d#hit_wall;
				end
			end
		) drones;



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
				| Do_Look(direction)            -> () (* List.iter (fun d -> self#look_one_drone d) drones *)
				
				(* TO DO ! create object 'bullet' with initial position the same as drone's *)				
				| Do_Shoot(direction, distance) -> self#add_bullet direction distance
													 
 			with Error_in_AI ("Main program terminated", "--", _) -> printf "%s: find call_stack is currently empty, moving on...\n" d#get_drone_name

		) drones;

		(* update position for all drones *)
		self#update_drone_position;

		(* TO DO! For all drones and bullets: update position, call GUI if needed *)
		!live_drones

end;;
