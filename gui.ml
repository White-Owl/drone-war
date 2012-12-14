
class gui =
object (self)
    val mutable info_y = 920
    method drawArena=
	Graphics.open_graph " 1200x1000";
        Graphics.set_window_title "Arena";
        Graphics.set_color (Graphics.black);
        Graphics.draw_rect 0 0 1000 1000;
    
    method drawDrone x y z=
	Graphics.set_color (Graphics.blue);
     	Graphics.draw_circle x y 6;
     	Graphics.moveto x y;
     	Graphics.lineto ( int_of_float(sin (z)*.12.) +x) (int_of_float(cos (z)*.12.)+y)

    method drawDroneDetail x y z name health=
	Graphics.set_color (Graphics.blue);
     	Graphics.draw_circle x y 6;
     	Graphics.moveto x y;
     	Graphics.lineto ( int_of_float(sin (z)*.12.) +x) (int_of_float(cos (z)*.12.)+y);
        if (x+50)>1000 then if (y+30)>1000 then Graphics.moveto (x-50) (y-13) else Graphics.moveto (x-50) (y+13) 
                       else if (y+30)>1000 then Graphics.moveto (x+13) (y-13) else Graphics.moveto (x+13) (y+13);
        Graphics.draw_string name;
        self#drawDroneHealth name health;
        if health=0 then (self#drawDroneDead x y);

    method drawDroneHealth name health=
        Graphics.set_color (Graphics.black);
        info_y <- (info_y-30);
        Graphics.moveto 1010 info_y;
	let outString=String.concat ": " [name;(string_of_int health)] in
            Graphics.draw_string outString;
    
    method drawDroneDead x y=
        Graphics.set_color (Graphics.red);
        Graphics.moveto (x-7) (y+7);
        Graphics.lineto (x+7) (y-7);
        Graphics.moveto (x-7) (y-7);
        Graphics.lineto (x+7) (y+7);

    method drawBullet x y=
	Graphics.set_color (Graphics.black);
        Graphics.fill_circle x y 4;
  
    method drawExplode x y=
     	Graphics.set_color (Graphics.red);
     	Graphics.fill_poly [|(x-9,y+3);(x-3,y+3);(x,y+9);(x+3,y+3);(x+9,y+3);(x+5,y-2);(x+6,y-9);(x,y-3);(x-6,y-9);(x-5,y-2)|];

    method clear=
	Graphics.clear_graph ();
        Graphics.set_color (Graphics.black);
        Graphics.draw_rect 0 0 1000 1000;
        info_y <- 920;

    method wait=
        let s = Graphics.wait_next_event [Graphics.Button_down;Graphics.Key_pressed] in if s.Graphics.button
          then Graphics.set_color (Graphics.red);

end;;



