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
	Array.iter (fun parameter ->
		if parameter.[0]='-' then
		begin
			print_endline ("option " ^parameter);
		end
		else
		if (Filename.check_suffix parameter ".dt" ) then
		begin
			print_string "Loading ";
			print_endline parameter;
			cage#load parameter;
			print_endline " ok";
		end
	) Sys.argv;

	print_string ("Loaded " ^ (string_of_int cage#get_drone_count) ^ " drones\n");
	exit 0;;

main;;
