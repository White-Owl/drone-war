open Drone;;
open Printf;;
open Bullet;;
open Ast;;

class arena =
object (self)
	val mutable drones = []
	val mutable bullets = []
	val mutable debug_mode = false

	val mutable pi = 4. *. atan 1.
	val mutable look_range = 30 		(*+30 and -30 on the given degree*)
	val mutable bullet_speed = 50
	val mutable drone_speed = 10

	val mutable area_map_x = 1000
	val mutable area_map_y = 1000

	val mutable team_counter = 0
	val mutable gathering_team = false

	method set_debug_mode mode =
		debug_mode <- mode

	method load file_name =
		let d = new drone in (
			d#load file_name;
			d#init; (*set random of drone's position*)
			d#belongs_to_team team_counter;
			if not gathering_team then team_counter <- team_counter+1;
			if debug_mode then begin
				let decompiled_file = open_out (file_name ^ ".decompiled") in
				d#decompile decompiled_file;
				close_out decompiled_file;
				d#set_debug_output (open_out (file_name ^ ".debug"));
			end;
			drones <- d :: drones;
		)

	method get_drone_count = List.length drones;


	method add_bullet dire dist x_pos y_pos =
		let b = new bullet in (
			begin
			b#set_direction dire;
			b#set_distance dist;
			b#set_x_position x_pos;
			b#set_y_position y_pos;
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
		int_of_float(sqrt((x1 -. x2)*.(x1 -. x2) +. (y1 -. y2)*.(y1 -. y2)))


   	method look_one_drone dire d_shoot d_target =
   	let 
   	d_shoot_x=d_shoot#get_x_position and
   	d_shoot_y=d_shoot#get_y_position and
   	d_target_x=d_target#get_x_position and
   	d_target_y=d_target#get_y_position in
			let target_dire = int_of_float(atan( (d_target_y -. d_shoot_y) /. (d_target_x  -. d_shoot_x)) *. 180. /. pi) and
				distance = self#get_distance d_target_x d_target_y d_shoot_x d_shoot_y
			in
			
			let flag=
			if(d_shoot#get_team_id=d_target#get_team_id)
			then Foe
			else Ally
			in
			if
				target_dire < (dire + look_range) && target_dire > (dire - look_range)
			then 
				d_shoot#add_found_target distance target_dire flag

	method look_wall dire d_look=
	let 
	d_look_x=d_look#get_x_position and
	d_look_y=d_look#get_y_position in
	let 
	wall_x=(float_of_int(area_map_y) -. d_look_y) /. tan(float_of_int(dire) /. 180. *. pi) +. d_look_x and
	wall_y=(float_of_int(area_map_x) -. d_look_x) *. tan(float_of_int(dire) /. 180. *. pi) +. d_look_y in
	let 
	dist = self#get_distance d_look_x d_look_y wall_x wall_y
	in
	d_look#add_found_target dist dire Wall 


	method update_drone_position =
		List.iter (fun d ->
			begin
			d#move drone_speed;
			if d#check_hit_wall = true
			then
				begin
				d#update_hit_pos;
				d#hit_wall;
				end;
			if d#get_moving_status = true
			then
				d#print_current_pos;
			end
		) drones;


	method update_bullet_position =
		List.iter (fun b ->
			begin
			b#move bullet_speed;
			if b#check_hit_wall = true
			then
				begin
					b#update_hit_pos;
					List.iter (fun d -> 
					self#explosion b d) drones;
				end;
			if b#check_reach_distance = true
			then
				begin
					List.iter (fun d -> 
					self#explosion b d) drones;
				end;
			end
		) bullets;

	
   	method explosion b d =
   	let 
   	d_x=d#get_x_position and
   	d_y=d#get_y_position and
   	exp_x=b#get_pos_x and
	exp_y=b#get_pos_y in
			let distance = self#get_distance d_x d_y exp_x exp_y
			in
			if distance < 50
			then 
			begin
				d#set_health (100 - distance);
				(*after GUI perform explosion, it should remove exploed bullet*)
				b#set_exploed true;
 			end;
				
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
				| Do_Look(direction)            -> 
				begin
				List.iter (fun dd -> self#look_one_drone direction d dd) drones;
				self#look_wall direction d;
				end
				(* TO DO ! create object 'bullet' with initial position the same as drone's *)
				| Do_Shoot(direction, distance) -> if distance > 500 then self#add_bullet direction 500 d#get_x_position d#get_y_position 
																	 else self#add_bullet direction distance d#get_x_position d#get_y_position 
 			with Error_in_AI ("Main program terminated", "--", _) -> printf "%s: find call_stack is currently empty, moving on...\n" d#get_drone_name

		) drones;

		(* update position for all drones *)
		self#update_drone_position;
		self#update_bullet_position;

		(* TO DO! For all drones and bullets: update position, call GUI if needed *)
		!live_drones

	method start_a_team =
		team_counter <- team_counter+1;
		gathering_team <- true

end;;
