exception Parse_failure of string * int * int;;

let pi = 4. *. atan 1.;;

let distance(x1, y1, x2, y2) =
	int_of_float(sqrt((x1 -. x2)*.(x1 -. x2) +. (y1 -. y2)*.(y1 -. y2)));;

let radian_of_degree angle =
	float_of_int(angle) *. pi /. 180.;;