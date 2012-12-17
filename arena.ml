open Drone;;
open Printf;;
open Bullet;;
open Ast;;
open Utils;;
open Gui;;

class arena =
object (self)
	val mutable drones : drone list = []
	val mutable bullets : bullet list = []
    val mutable arena_gui = new gui
	val mutable debug_mode = false

	val mutable look_range = 30 		(* +30 and -30 on the given degree *)
	val mutable bullet_speed = 50
	val mutable drone_speed = 10

	val mutable area_map_x = 1000
	val mutable area_map_y = 1000

	val mutable team_counter = 0
	val mutable gathering_team = false

	method set_debug_mode mode = debug_mode <- mode

	method load file_name =
		let d = new drone in begin
			d#load file_name;
			d#belongs_to_team team_counter;
			if not gathering_team then team_counter <- team_counter+1;
			if debug_mode then begin
				let decompiled_file = open_out (file_name ^ ".decompiled") in
				d#decompile decompiled_file;
				close_out decompiled_file;
				d#set_debug_output (open_out (file_name ^ ".debug"));
			end;
			drones <- d :: drones
		end

	method get_drone_count = List.length drones;


	method add_bullet dist dire shoot_d =
		let b = new bullet in
		b#init shoot_d#get_x_position shoot_d#get_y_position dire dist;
		bullets <- b :: bullets


	method run =
                arena_gui#drawArena;
		let steps = ref 1 in
		while (self#step > 1) && (!steps < 1000) do
			incr steps
		done;
		printf "Results:\n";
		List.iter (fun d ->
			printf "%s: %s\n" d#get_drone_name
				(if d#is_brain_dead then
					("brain dead after " ^ (string_of_int d#get_ai_ticks) ^ " ticks with explanation: " ^ d#get_reason_for_coma)
				 else if not d#is_alive then
					("died after " ^ (string_of_int d#get_ai_ticks) ^ " ticks")
				 else
					("still alive with " ^ (string_of_int d#get_health) ^ "% of health ")
				)
		) drones


   	method look_one_drone dire d_shoot d_target =
	   	let	d_shoot_x=d_shoot#get_x_position and d_shoot_y=d_shoot#get_y_position and
				d_target_x=d_target#get_x_position and d_target_y=d_target#get_y_position in
		let target_dire = int_of_float(atan( (d_target_y -. d_shoot_y) /. (d_target_x  -. d_shoot_x)) *. 180. /. pi) and
				dist = distance (d_target_x, d_target_y, d_shoot_x, d_shoot_y)	in
		let flag=(if (d_shoot#get_team_id=d_target#get_team_id) then Ally else Foe) in
		if (target_dire < (dire + look_range)) && (target_dire > (dire - look_range)) && (not (d_shoot == d_target) && (d_target#is_alive = true))
			then d_shoot#found_target dist target_dire flag


	(* get a distance to the wall in the exact direction of the drone's look *)
	 method look_wall dire d_look=
		let x=d_look#get_x_position and y=d_look#get_y_position in
		let md = dire mod 360 in
		let rd = radian_of_degree md in
		let dh = max (int_of_float ((0. -. x) /. (cos rd))) (int_of_float ((1000. -. x) /. (cos rd))) in
		let dv = max (int_of_float ((0. -. y) /. (sin rd))) (int_of_float ((1000. -. y) /. (sin rd))) in
		let dist = if md=0 || md=180 then dh
					else if md=90 || md=270 then dv
					else min dh dv in
        d_look#found_target dist dire Wall


 (*  That method is extremly complicated and therefore prone to mistakes... *)
	(* method look_wall2 dire d_look=
		let d_look_x=d_look#get_x_position and d_look_y=d_look#get_y_position in
		let	k=tan(float_of_int(dire)) in
		let	intercept=d_look_x -. k *. d_look_y in
		let mod_dire=dire mod 360 in
		let (wall_x, wall_y )=
			if mod_dire > 0 && mod_dire<=90 then
				if (float_of_int(area_map_y)) *. k +. intercept > ((float_of_int(area_map_x)) -. intercept )/. k
				then
				 	((float_of_int(area_map_x)),((float_of_int(area_map_x)) -. intercept )/. k)
				else
				 	(((float_of_int(area_map_y)) -. intercept )/. k,(float_of_int(area_map_y)))
			else if mod_dire > 90 && mod_dire<=180 then
				if (float_of_int(area_map_y)) *. k +. intercept<0.
				then
				 	(0. ,(0. -. intercept )/. k)
				else
				 	(((float_of_int(area_map_y)) -. intercept )/. k,(float_of_int(area_map_y)))
			else if mod_dire > 180 && mod_dire<=270 then
				if  intercept > 0.
				then
				 	(intercept,0.)
				else
				 	(0.,intercept /.(0.-.k))
			else
				if  intercept >float_of_int(area_map_x)
				then
					(float_of_int(area_map_x),(float_of_int(area_map_x)-.intercept)/.k)
				else
				 	(intercept,0.)
		in
		let dist = distance (d_look_x, d_look_y, wall_x, wall_y) in
		d_look#found_target dist dire Wall  *)




   	method explosion b d =
   		let d_x=d#get_x_position and d_y=d#get_y_position and exp_x=b#get_pos_x and exp_y=b#get_pos_y in
		let dist = distance (d_x, d_y, exp_x, exp_y) in
		if dist < 50 then d#set_health (d#get_health - 50 + dist)



	method step =
		let live_drones = ref 0 in 		(* to check how many drones are still alive and kicking *)
		List.iter (fun drone ->
			if (drone#is_alive) && (not drone#is_brain_dead) then begin
				incr live_drones;
				try (
					let action = drone#step in
					match action with
					  No_Action                     -> ()
					| Do_Shoot(direction, distance) -> self#add_bullet distance direction drone
					| Do_Look(direction)            -> 	begin
														drone#found_target 0 0 End;
														self#look_wall direction drone;
														List.iter (fun dd -> self#look_one_drone direction drone dd) (List.rev(self#sort_by_dist drone drones));
														end
				)
 				with Error_in_AI (reason, sub, position) -> printf "Drone %s died at %s:%d with explanation: %s\n" drone#get_drone_name sub position reason
			end
		) drones;
		(* update position for all drones and bullets *)
		List.iter (fun d -> d#move drone_speed ) drones;
		List.iter (fun b -> b#move bullet_speed; if b#is_exploded then List.iter(fun d -> self#explosion b d) drones) bullets;

 		List.iter (fun d -> d#print_current_pos ) drones;
		(* TO DO! call GUI if needed *)
                arena_gui#clear;
                List.iter (fun d -> arena_gui#drawDroneDetail (int_of_float d#get_x_position) (int_of_float d#get_y_position) (radian_of_degree d#get_moving_direction) (radian_of_degree d#get_direction_of_the_gun) d#get_drone_name d#get_health d#get_team_id d#get_ai_ticks d#get_moving_status d#get_reason_for_coma d#get_gun_cooldown) drones;
                List.iter (fun b -> if(b#is_exploded) then arena_gui#drawExplode (int_of_float b#get_pos_x) (int_of_float b#get_pos_y)else arena_gui#drawBullet (int_of_float b#get_pos_x) (int_of_float b#get_pos_y)) bullets;
                arena_gui#wait;
		(* remove all exploded bullets from the arena *)
		bullets <- List.filter (fun b -> not b#is_exploded) bullets;
		!live_drones 


	method ins d drone d_list =
		let rec insert d e elements =
			match elements with
			  [] -> [e]
			| head :: tail -> if distance (d#get_x_position, d#get_y_position, e#get_x_position, e#get_y_position) <=
								 distance (head#get_x_position, head#get_y_position, d#get_x_position, d#get_y_position)
							  then e :: elements
							  else head :: insert d e tail
			in
		insert d drone d_list

	method sort_by_dist d d_list=
		let rec sort d elements =
			match elements with
			  [] -> []
			| head :: tail -> self#ins d head (sort d tail)
		in
		sort d d_list


	method start_a_team =
		team_counter <- team_counter+1;
		gathering_team <- true

end;;
