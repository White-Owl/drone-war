open Drone;;

class arena =
object
	val mutable drones = []

	method load file_name =
		let d = new drone in
		if (d#load file_name ) then (
			drones <- d::drones;
			true;
		) else
			false;

	method get_drone_count = List.length drones;

end;;
