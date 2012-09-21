open Arena;;

let main =
	print_string "The Drone War\nThe class project for COMS W4115 Programming Languages and Translators\nColumbia University, Fall 2012\n\
				  Professor:\tStephen A. Edwards\n\
				  Students:\tGeorge Brink (gb2280)\n\
				  \t\tXiang Yao (xy2191)\n\
				  \t\tXiaotong Chen (xc2230)\n\
				  \t\tShuo Qin (sq2144)\n\n\
				  ";

	let cage = new arena in
(*	Array.iter (fun parameter ->
		if Filename.check_suffix(parameter, ".dt") then (
			print_string "Loading ";
			print_string (parameter ^ "\n");
			cage#load (parameter);
		) else (
			print_string "failed\n"
		)
	) Sys.argv; *)

	for i = 1 to (Array.length Sys.argv) - 1 do
		print_string "Loading ";
		print_string (Sys.argv.(i) ^ "\n");
		cage#load (Sys.argv.(i));
	done;
	print_string ("Loaded " ^ (string_of_int cage#get_drone_count) ^ " drones\n");
	exit 0;;

main;;
