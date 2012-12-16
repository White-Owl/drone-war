
class gui =
object (self)
    val mutable info_x = 0
    val mutable info_y = 0
    val mutable size_x = 0
    val mutable size_y = 0
    val mutable max_x = 0
    val mutable max_y = 0
    val mutable temp_x = 0
    val mutable temp_y = 0
    val mutable counter = 0
    method drawArena=
	Graphics.open_graph " 1000x1000";
        Graphics.set_window_title "Arena";
        Graphics.set_color (Graphics.black);
        max_x <- Graphics.size_x();
        max_y <- Graphics.size_y();
        info_x <-(max_x-190);
        info_y <-(max_y-25);
        size_x <-(max_x-220);
        size_y <-(max_y-40);
        Graphics.draw_rect 20 20 size_x size_y;
        Graphics.moveto info_x (max_y-30);
        Graphics.draw_string "Total Ticks: ";
        Graphics.draw_string (string_of_int counter);
    
    method translate x y=
	temp_x <- (20 + x * size_x / 1000);
        temp_y <- (20 + y * size_y / 1000);
     
    method drawDrone x y z=
	Graphics.set_color (Graphics.blue);
        self#translate x y;
     	Graphics.draw_circle temp_x temp_y 6;
     	Graphics.moveto temp_x temp_y;
     	Graphics.lineto ( int_of_float(cos (z)*.12.) +size_x) (int_of_float(sin (z)*.12.)+temp_y);
        

    method drawCircleDroneDetail x y z name health=
	Graphics.set_color (Graphics.blue);
        self#translate x y;
     	Graphics.draw_circle temp_x temp_y 6;
     	Graphics.moveto temp_x temp_y;
     	Graphics.lineto ( int_of_float(cos (z)*.12.) +temp_x) (int_of_float(sin (z)*.12.)+temp_y);
        if (x+String.length(name))>1000 then if (y+30)>1000 then self#translate (x-50) (y-13) else self#translate (x-50) (y+13)
                       else if (y+30)>1000 then self#translate (x+13) (y-13) else self#translate (x+13) (y+13);
        Graphics.moveto temp_x temp_y;
        Graphics.draw_string name;
        (*self#drawDroneHealth name health;*)
        if health=0 then (self#drawDroneDead x y);
        
    method drawDroneDetail x y body_direc gun_direc name health team_id ai_ticks moving_status reason_for_coma gun_cooldown=
        self#drawDroneColor team_id;
        self#translate x y;
        self#drawDroneBody x y body_direc;                (*draw the body of the drone *)
     	Graphics.moveto temp_x temp_y;
     	Graphics.lineto ( int_of_float(cos (gun_direc)*.15.) +temp_x) (int_of_float(sin (gun_direc)*.15.)+temp_y);       (*draw the gun of the drone *)
        if (x+7*String.length(name))>1000 then if (y+30)>1000 then self#translate (x-7*String.length(name)) (y-23) else self#translate (x-7*String.length(name)) (y+16)
                       else if (y+30)>1000 then self#translate (x+13) (y-23) else self#translate (x+13) (y+16);
        Graphics.moveto temp_x temp_y;
        Graphics.draw_string name;          (*draw the name of the drone *)
        Graphics.moveto temp_x (temp_y-10);
        Graphics.draw_string (string_of_int health);          (*draw the name of the drone *)
        self#drawDroneInfo name health team_id ai_ticks moving_status reason_for_coma gun_cooldown;    (*draw the information of the drone *)
        if health=0 then (self#drawDroneDead x y);       (*draw the deadbody of the drone *)

    method drawDroneBody x y body_direc=
        self#translate x y;
        let pi = 4. *. atan 1. in
        let x1=int_of_float(cos (body_direc)*.10.) +temp_x in
        let y1=int_of_float(sin (body_direc)*.10.) +temp_y in
        let x2=int_of_float(cos (body_direc +. (140.*. pi /.180.))*.10.) +temp_x in
        let y2=int_of_float(sin (body_direc +. (140.*. pi /.180.))*.10.) +temp_y in
        let x3=int_of_float(cos (body_direc +. (220.*. pi /.180.))*.10.) +temp_x in
        let y3=int_of_float(sin (body_direc +. (220.*. pi /.180.))*.10.) +temp_y in
        Graphics.draw_poly [|(x1,y1);(x2,y2);(x3,y3)|];

    method drawDroneInfo name health team_id ai_ticks moving_status reason_for_coma gun_cooldown=
        Graphics.set_color (10494192);
        info_y <- (info_y-15);
        Graphics.moveto info_x info_y;
        Graphics.draw_string name;
        Graphics.set_color (Graphics.black);
        info_y <- (info_y-10);
        Graphics.moveto info_x info_y;
        Graphics.draw_string "Team ID: ";
        Graphics.draw_string (string_of_int team_id);
        info_y <- (info_y-10);
        Graphics.moveto info_x info_y;
        Graphics.draw_string "Health: ";
        Graphics.draw_string (string_of_int health);
        info_y <- (info_y-10);
        Graphics.moveto info_x info_y;
        Graphics.draw_string "AI Ticks: ";
        Graphics.draw_string (string_of_int ai_ticks);
        info_y <- (info_y-10);
        Graphics.moveto info_x info_y;
        Graphics.draw_string "Moving: ";
        Graphics.draw_string (string_of_bool moving_status);
        info_y <- (info_y-10);
        Graphics.moveto info_x info_y;
        Graphics.draw_string "Reason for coma: ";
        if reason_for_coma="" then Graphics.draw_string "Not coma yet" else Graphics.draw_string reason_for_coma;
        info_y <- (info_y-10);
        Graphics.moveto info_x info_y;
        Graphics.draw_string "Gun cooldown: ";
        Graphics.draw_string (string_of_int gun_cooldown);
    
    method drawDroneDead x y=
        Graphics.set_color (Graphics.red);
        self#translate x y;
        Graphics.moveto (temp_x-7) (temp_y+7);
        Graphics.lineto (temp_x+7) (temp_y-7);
        Graphics.moveto (temp_x-7) (temp_y-7);
        Graphics.lineto (temp_x+7) (temp_y+7);

    method drawDroneColor x=
        match x with 
          0 ->  Graphics.set_color (Graphics.red)
          |1 -> Graphics.set_color (Graphics.green)
          |2 -> Graphics.set_color (Graphics.blue)
          |3 -> Graphics.set_color (10506797)
          |4 -> Graphics.set_color (Graphics.cyan)
          |5 -> Graphics.set_color (Graphics.magenta)
          |6 -> Graphics.set_color (16744228)
          |7 -> Graphics.set_color (16759055)
          |8 -> Graphics.set_color (13487360)
          |9 -> Graphics.set_color (13445520)
          |10 -> Graphics.set_color (12092939)
          |11 -> Graphics.set_color (9005261)
          |12 -> Graphics.set_color (9132544)
          |13 -> Graphics.set_color (5577355)
          |14 -> Graphics.set_color (128)
          |_ -> Graphics.set_color (Graphics.black)

    method drawBullet x y=
	Graphics.set_color (Graphics.black);
        self#translate x y;
        Graphics.fill_circle temp_x temp_y 4;
  
    method drawExplode x y=
     	Graphics.set_color (Graphics.red);
        self#translate x y;
     	Graphics.fill_poly [|(temp_x-27,temp_y+9);(temp_x-9,temp_y+9);(temp_x,temp_y+27);(temp_x+9,temp_y+9);(temp_x+27,temp_y+9);(temp_x+15,temp_y-6);(temp_x+18,temp_y-27);(temp_x,temp_y-12);(temp_x-18,temp_y-27);(temp_x-15,temp_y-6)|];

    method clear=
	Graphics.clear_graph ();
        Graphics.set_color (Graphics.black);
        Graphics.draw_rect 20 20 size_x size_y;  
        info_y <- (max_y-25);
        counter <- (counter+1);
        Graphics.moveto info_x (max_y-30);
        Graphics.draw_string "Total Ticks: ";
        Graphics.draw_string (string_of_int counter);

    method wait=
        let s = Graphics.wait_next_event [Graphics.Button_down;Graphics.Key_pressed] in if s.Graphics.button
          then Graphics.set_color (Graphics.red);

end;;



