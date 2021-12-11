#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open_graph "800x600";;

type point   = {mutable x: float;  mutable y: float; mutable oldx:float; mutable oldy:float};;

auto_synchronize false;;
set_color (rgb 255 0 255);;

let vx= ref 0.;;
let vy =ref 0.;;

let pt = ref {x = 100.; y = 100.;oldx=99.;oldy=99.}
in while true do
	set_color (rgb 255 255 255);
	fill_circle (int_of_float !pt.x) (int_of_float !pt.y) 20;
	set_color (rgb 255 0 255);
	vx:= !pt.x -. !pt.oldx ;
	vy:= !pt.y -. !pt.oldy ;
	pt := {x= !pt.x +. !vx; y= !pt.y +. !vy; oldx= !pt.x; oldy= !pt.y  };
	if (!pt.x>800. ) then begin !pt.x <- 800.; !pt.oldx<- !pt.x +. !vx;end
	else if (!pt.x<0. ) then begin !pt.x <- 0.; !pt.oldx<- !pt.x +. !vx;end;
	if (!pt.y>600. ) then begin !pt.y <- 600.; !pt.oldy<- !pt.y +. !vy;end
	else if(!pt.y<0. ) then begin !pt.y <- 0.; !pt.oldy<- !pt.y +. !vy;end;
		draw_circle (int_of_float !pt.x) (int_of_float !pt.y) 20;
		fill_circle (int_of_float !pt.x) (int_of_float !pt.y) 20;
		synchronize ();
		
		done;;
}