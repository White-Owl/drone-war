open Drone;;

class arena =
object
	val mutable drones = []

	method load file_name =
		let d = new drone in (
			d#load file_name;
			drones <- d::drones;
			print_newline();
			d#decompile;
			d#run d#get_current_sub;
			d#prt;
		)

	method get_drone_count = List.length drones;


	  
end;;
