open Utils;;

class bullet =
	object (self)

	val mutable direction = 0
	val mutable x_position = 0.
	val mutable y_position = 0.
	val mutable distance_to_fly = 0
	val mutable distance_traveled = 0

	val mutable start_x_position = 0.
	val mutable start_y_position = 0.
	val mutable exploded = false

	method get_pos_x = x_position

	method get_pos_y = y_position

	method get_direction = direction

	method is_exploded = exploded

	method init x y dir dist =
		start_x_position <- x;
		x_position <- x;
		start_y_position <- y;
		y_position <- y;
		direction <- dir;
		distance_to_fly <- min dist 500


	method move speed =
		y_position <- y_position +. (float_of_int(speed) *. (sin (float_of_int(direction) *. pi /. 180.)));
		x_position <- x_position +. (float_of_int(speed) *. (cos (float_of_int(direction) *. pi /. 180.)));
		distance_traveled <- distance(x_position, y_position, start_x_position, start_y_position);
		exploded <- (x_position > 1000.) || (x_position < 0.) || (y_position > 1000.) || (y_position < 0.);
		if exploded
			then self#update_position_if_flew_out_of_arena
			else exploded <- distance_traveled >= distance_to_fly


	method update_position_if_flew_out_of_arena =
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
			print_int direction;
			print_endline "";
			print_endline "";
		end *)

end;;