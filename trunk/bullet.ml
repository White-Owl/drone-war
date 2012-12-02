
class bullet =
	object (self)

	val mutable direction_of_the_body = 0;
	val mutable x_position = 0.;
	val mutable y_position = 0.;
	val mutable distance = 0;

	val mutable start_x_position = 0.;
	val mutable start_y_position = 0.;
	val mutable pi = 4. *. atan 1.

	val mutable explored = false;

	method get_pos_x = x_position

	method get_pos_y = y_position

	method get_direction = direction_of_the_body

	method set_x_position x = x_position <- x

	method set_y_position y = y_position <- y 

	method set_direction dire = direction_of_the_body <- dire

	method set_distance dis = distance <- dis;

	method get_exploed = explored

	method set_exploed exp = explored <- exp


	method move speed =
			let mod_dire = (direction_of_the_body mod 360) in
			begin
				if mod_dire >= 0 && mod_dire <= 90
				then
					y_position <- y_position +. (float_of_int(speed) *. (cos (float_of_int(mod_dire) *. pi /. 180.)));
					x_position <- x_position +. (float_of_int(speed) *. (sin (float_of_int(mod_dire) *. pi /. 180.)));
				if mod_dire > 90 && mod_dire <= 180
				then
					y_position <- y_position +. (float_of_int(speed) *. (sin (float_of_int(180 - mod_dire) *. pi /. 180.)));
					x_position <- x_position -. (float_of_int(speed) *. (cos (float_of_int(180 - mod_dire) *. pi /. 180.)));

				if mod_dire > 180 && mod_dire <= 270
				then
					y_position <- y_position -. (float_of_int(speed) *. (cos (float_of_int(270 - mod_dire) *. pi /. 180.)));
					x_position <- x_position -. (float_of_int(speed) *. (sin (float_of_int(270 - mod_dire) *. pi /. 180.)));

				if mod_dire > 270 && mod_dire < 360 
				then
					y_position <- y_position -. (float_of_int(speed) *. (sin (float_of_int(360 - mod_dire) *. pi /. 180.)));
					x_position <- x_position +. (float_of_int(speed) *. (cos (float_of_int(360 - mod_dire) *. pi /. 180.)));
			end


	method cal_distance x1 y1 x2 y2 = 
		int_of_float(sqrt((x1 -. x2)*.(x1 -. x2) +. (y1 -. y2)*.(y1 -. y2)))

	method check_reach_distance =
		let dis = self#cal_distance x_position y_position start_x_position start_y_position in
		if  dis >= distance
		then true
		else false


	method check_hit_wall = 
		if x_position > 1000. || x_position < 0. || y_position > 1000. || y_position < 0.
		then true
		else false

	method update_hit_pos =
		begin
			if x_position > 1000. then x_position <- 1000.;
			if x_position < 0. then x_position <- 0.;
			if y_position > 1000. then y_position <- 1000.;
			if y_position < 0. then y_position <- 0.;
		end

	(* method print_current_pos = 
		begin
			print_float x_position;
			print_endline "";
			print_float y_position;
			print_endline "";
			print_int direction_of_the_body;
			print_endline "";
			print_endline "";
		end *)

end;;