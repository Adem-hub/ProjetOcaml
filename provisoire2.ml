#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open Array;;
open_graph "800x600";;

type point   = {mutable x: float;  mutable y: float; mutable oldx:float; mutable oldy:float};;


let vx= ref 0.;;
let vy =ref 0.;;


auto_synchronize false;;
set_color (rgb 255 0 255);;

let vx= ref 0.;;
let vy =ref 0.;;
let rebond=1. and 
 pt = ref [|{x = 400.; y = 300.;oldx=399.;oldy=299.};{x = 200.; y = 100.;oldx=199.;oldy=99.};{x = 300.; y = 100.;oldx=299.;oldy=99.}|]
in while true do
	for i=0 to (length !pt)-1 do
		set_color (rgb 255 255 255);
		fill_circle (int_of_float !pt.(i).x) (int_of_float !pt.(i).y) 20;
		set_color (rgb 255 0 255);
		vx:= !pt.(i).x -. !pt.(i).oldx ;
		vy:= !pt.(i).y -. !pt.(i).oldy ;
		!pt.(i) <- {x= !pt.(i).x +. !vx; y= !pt.(i).y +. !vy -. 0.1; oldx= !pt.(i).x; oldy= !pt.(i).y  };
		if (!pt.(i).x>800. ) then begin !pt.(i).x <- 800.; !pt.(i).oldx<- !pt.(i).x +. !vx *. rebond;end
		else if (!pt.(i).x<0. ) then begin !pt.(i).x <- 0.; !pt.(i).oldx<- !pt.(i).x +. !vx *. rebond;end;
		if (!pt.(i).y>600. ) then begin !pt.(i).y <- 600.; !pt.(i).oldy<- !pt.(i).y +. !vy *. rebond;end
		else if(!pt.(i).y<0. ) then begin !pt.(i).y <- 0.; !pt.(i).oldy<- !pt.(i).y +. !vy *. rebond;end;
			draw_circle (int_of_float !pt.(i).x) (int_of_float !pt.(i).y) 20;
			fill_circle (int_of_float !pt.(i).x) (int_of_float !pt.(i).y) 20;
			synchronize ();
		done;
		done;;
}