open Ast;;
open Parser;;
open Printf;;
open Drone;;

class bullet =
	object (self)

	val mutable direction_of_the_body = 0;
	val mutable current_position_x = 0;
	val mutable current_position_y = 0;
	val mutable drone_name = "";
	val mutable distance = 0;

	method get_pos_x = current_position_x

	method get_pos_y = current_position_y

	method get_direction = direction_of_the_body

	method set_direction dire = direction_of_the_body <- dire

	method set_distance dis = distance <- dis;

end;;