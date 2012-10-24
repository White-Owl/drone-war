open Drone;;

class arena =
object
	val mutable drones = []

	method load file_name =
		let d = new drone in (
			d#load file_name;
			drones <- d::drones;
			print_newline();
			d#dump_code;
		)

	method get_drone_count = List.length drones;

end;;
