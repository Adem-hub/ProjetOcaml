#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open Array;;

type point   = {mutable x: float;  mutable y: float; mutable oldx:float; mutable oldy:float};;
type stick = {mutable debut: point;  mutable fin: point; mutable taille:float};;
let dist (x1,y1) (x2,y2) = sqrt((x1-.x2)**2. +. (y1-.y2)**2.);;

let update lien = 
	let dx= lien.fin.x -. lien.debut.x 
	and dy= lien.fin.y -. lien.debut.y in
	let distance = sqrt( dx**2. +. dy**2.) in
	let diff = lien.taille -. distance in 
	let pourcent = diff /. distance /.2. in
	let decX = dx *. pourcent
	and decY = dy *. pourcent in
		lien.debut.x <- (lien.debut.x) -. decX;
		lien.debut.y <- (lien.debut.y) -. decY;
		lien.fin.x   <- (lien.fin.x) +. decX;
		lien.fin.y   <- (lien.fin.y) +. decY;;



set_color (rgb 255 0 255);;


open_graph "800x600";
auto_synchronize false;
display_mode false;
let vx     = ref 0.
and vy     = ref 0.
and rebond = 1. 
and pt = ref [|{x = 100.; y = 100.;oldx=99.;oldy=99.};
					{x = 200.; y = 200.;oldx=199.;oldy=199.};
					{x = 300.; y = 100.;oldx=299.;oldy=99.}|] in
let st = {debut = (!pt).(0) ; fin= (!pt).(1) ; taille= dist ((!pt.(0)).x,(!pt.(0)).y) ((!pt.(1)).x,(!pt.(1)).y)}in
while true do
	for i=0 to (length !pt)-2 do
		vx:= !pt.(i).x -. !pt.(i).oldx ;
		vy:= !pt.(i).y -. !pt.(i).oldy ;
		!pt.(i) <- {x= !pt.(i).x +. !vx; y= !pt.(i).y +. !vy -. 0.01; oldx= !pt.(i).x; oldy= !pt.(i).y  };
		if (!pt.(i).x>800. ) then
			begin
			!pt.(i).x <- 800.;
			!pt.(i).oldx <- !pt.(i).x +. !vx *. rebond;
			end
		else if (!pt.(i).x<0. ) then
			begin
			!pt.(i).x <- 0.;
			!pt.(i).oldx<- !pt.(i).x +. !vx *. rebond;
			end;
		if (!pt.(i).y>600. ) then
			begin
			!pt.(i).y <- 600.;
			!pt.(i).oldy<- !pt.(i).y +. !vy *. rebond;
			end
		else if(!pt.(i).y<0. ) then
			begin
			!pt.(i).y <- 0.;
			!pt.(i).oldy <- !pt.(i).y +. !vy *. rebond;
			end;
		draw_circle (int_of_float !pt.(i).x) (int_of_float !pt.(i).y) 20;
		fill_circle (int_of_float !pt.(i).x) (int_of_float !pt.(i).y) 20;
		moveto (int_of_float st.debut.x) (int_of_float st.debut.y);
		lineto (int_of_float st.fin.x) (int_of_float st.fin.y);
		if i=0 then st.debut<- !pt.(i) else  st.fin<- !pt.(i);
	done;
	synchronize ();
	clear_graph();
	update st;
	!pt.(0) <- st.debut ;
	!pt.(1) <- st.fin;
	st.taille <-  dist ((!pt.(0)).x,(!pt.(0)).y) ((!pt.(1)).x,(!pt.(1)).y);
done;;
