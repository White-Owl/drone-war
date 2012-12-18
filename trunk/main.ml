open Arena;;
open Printf;;
open Utils;;


let main =
	print_string "The Drone War\nThe class project for COMS W4115 Programming Languages and Translators\nColumbia University, Fall 2012\n\
				  Professor:\tStephen A. Edwards\n\
				  Students:\tGeorge Brink (gb2280)\n\
				  \t\tXiang Yao (xy2191)\n\
				  \t\tXiaotong Chen (xc2230)\n\
				  \t\tShuo Qiu (sq2144)\n\n\
				  ";
	Random.self_init();
	let cage = new arena in
	Array.iter (fun parameter ->
		if parameter.[0]='-' then
		begin
			match parameter.[1] with
			  'D' -> cage#set_debug_mode true
			| 't' -> cage#start_a_team
			| 'q' -> cage#disable_gui
			|  _  -> print_endline ("Unknown option " ^parameter);
		end
		else
		if (Filename.check_suffix parameter ".dt" ) || (Filename.check_suffix parameter ".dbt" )  then
		begin
			print_string "Loading ";
			print_string parameter;
			Random.self_init();
			try
				cage#load parameter;
				printf " - ok\n"
			with
			  Failure t            -> printf " - failed\n%s\n" t
            | Parse_failure(t,l,c) -> printf " - failed\n%s at %d:%d\n" t l c
			| Sys_error t          -> printf " - file error\n%s\n" t
		end
	) Sys.argv;

	print_string ("Loaded " ^ (string_of_int cage#get_drone_count) ^ " drones\n");
	cage#run;
	exit 0;;

main;;
