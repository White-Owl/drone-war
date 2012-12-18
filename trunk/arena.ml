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
	val mutable gui_enabled = true
	val mutable debug_mode = false

	val mutable look_range = 30 		(* +30 and -30 on the given degree *)
	val mutable bullet_speed = 5
	val mutable drone_speed = 1

	val mutable area_map_x = 1000
	val mutable area_map_y = 1000

	val mutable team_counter = 0
	val mutable gathering_team = false

	method disable_gui = gui_enabled<-false

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
				d#set_debug_output (open_out (file_name ^ ".debug"))
			end;
			drones <- d :: drones
		end

	method get_drone_count = List.length drones;


	method add_bullet dist dire shoot_d =
		let b = new bullet in
		b#init shoot_d#get_x_position shoot_d#get_y_position dire dist;
		bullets <- b :: bullets


	method run =
		if gui_enabled then arena_gui#drawArena;
		let steps = ref 1 in
		while (self#step > 1) && (!steps < 2000) do
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




   	method explosion b d =
   		let d_x=d#get_x_position and d_y=d#get_y_position and exp_x=b#get_pos_x and exp_y=b#get_pos_y in
		let dist = distance (d_x, d_y, exp_x, exp_y) in
		if dist < 50 then d#set_health (d#get_health - 50 + dist)



	method step =
		let live_drones = ref 0 in 		(* to check how many drones are still alive and kicking *)
		List.iter (fun active_drone ->
			if (active_drone#is_alive) && (not active_drone#is_brain_dead) then begin
				incr live_drones;
				try (
					let action = active_drone#step in
					match action with
					  No_Action                     -> ()
					| Do_Shoot(direction, distance) -> self#add_bullet distance direction active_drone
					| Do_Look(direction)            -> 	begin
														self#look_wall direction active_drone; (* the wall is always visible, and it is always the farthest object from the active drone *)
														let found_drones = List.filter (fun d ->
															if d==active_drone then false     (* the drone cannot see itself *)
															else if not d#is_alive then false (* ignore dead drones *)
															else begin  (* check if the drone is in the look range *)
																let angle_to_drone = degree_of_radian (atan2 (d#get_y_position -. active_drone#get_y_position)  (d#get_x_position -. active_drone#get_x_position) ) in
																abs (direction - angle_to_drone)  < look_range
															end
														) drones in
														(* sort all drones in the look range by the distance from the active drone *)
														let sorted_found_drones = List.rev(self#sort_by_dist active_drone found_drones) in
														(* add all found drones into the active drone's stack *)
														List.iter (fun d -> active_drone#found_target (distance(active_drone#get_x_position, active_drone#get_y_position, d#get_x_position, d#get_y_position))
																			                          (degree_of_radian (atan2 (d#get_y_position -. active_drone#get_y_position)  (d#get_x_position -. active_drone#get_x_position) ))
																			                          (if active_drone#get_team_id=d#get_team_id then Ally else Foe)
																  ) sorted_found_drones
														end
				)
 				with Error_in_AI (reason, sub, position) -> printf "Drone %s died at %s:%d with explanation: %s\n" active_drone#get_drone_name sub position reason
			end
		) drones;
		(* update position for all drones and bullets *)
		List.iter (fun d -> d#move drone_speed ) drones;
		List.iter (fun b -> b#move bullet_speed; if b#is_exploded then List.iter(fun d -> self#explosion b d) drones) bullets;

(*  		List.iter (fun d -> d#print_current_pos ) drones;
 *)		if gui_enabled then begin
                arena_gui#clear;
                List.iter (fun d -> arena_gui#drawDroneDetail (int_of_float d#get_x_position) (int_of_float d#get_y_position) (radian_of_degree d#get_moving_direction) (radian_of_degree d#get_direction_of_the_gun) d#get_drone_name d#get_health d#get_team_id d#get_ai_ticks d#get_moving_status d#get_reason_for_coma d#get_gun_cooldown) drones;
                List.iter (fun b -> if(b#is_exploded) then arena_gui#drawExplode (int_of_float b#get_pos_x) (int_of_float b#get_pos_y) else arena_gui#drawBullet (int_of_float b#get_pos_x) (int_of_float b#get_pos_y)) bullets;
                (*arena_gui#wait; *)
		end;
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
