#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open Array;;
open_graph " 800x600";;


type point   = {mutable x: float;  mutable y: float; mutable oldx:float; mutable oldy:float};;
type stick = {mutable debut: point;  mutable fin: point; mutable taille:float};;
let dist (x1,y1) (x2,y2) = sqrt((x1-.x2)**2. +. (y1-.y2)**2.);;

let update lien = 
	for i=0 to (length lien)-1 do 
		let dx= lien.(i).fin.x -. lien.(i).debut.x 
		and dy= lien.(i).fin.y -. lien.(i).debut.y in
		let distance = sqrt( dx**2. +. dy**2.) in
		let diff = lien.(i).taille -. distance in 
		let pourcent = diff /. distance /.2. in
		let decX = dx *. pourcent
		and decY = dy *. pourcent in
			lien.(i).debut.x <- (lien.(i).debut.x) -. decX;
			lien.(i).debut.y <- (lien.(i).debut.y) -. decY;
			lien.(i).fin.x   <- (lien.(i).fin.x) +. decX;
			lien.(i).fin.y   <- (lien.(i).fin.y) +. decY;
		done;;

let lecture_png nomFichier height width=
	let fichier  = open_in nomFichier
	and couleurs = Array.make_matrix height width transp in
		for i = 0 to height - 1 do
			for j = 0 to width - 1 do
				let ligne = input_line fichier in
				let r, g, b, a = Scanf.sscanf ligne "%d %d %d %d" (fun a b c d -> (a, b, c, d)) in
				if a < 125 then
					begin
					couleurs.(i).(j) <- (transp);
					end
				else
					begin
					couleurs.(i).(j) <- (rgb r g b);
					end;
			done;
  		done;
  		couleurs;;
  		
let raw_image = lecture_png "C:\\Users\\abenr\\OneDrive\\Bureau\\ProjetOcaml-main\\ball.txt" 50 50;;

let image = make_image raw_image;;



auto_synchronize false;
display_mode false;
let vx     = ref 0.
and vy     = ref 0.
and rebond = 0.5 
and pt = ref [|{x = 100.; y = 100.;oldx=99.;oldy=99.};
					{x = 100.; y = 400.;oldx=99.;oldy=399.};
					{x = 400.; y = 100.;oldx=399.;oldy=199.};
					{x = 400.; y = 400.;oldx=399.;oldy=399.}|] in
let st = [|{debut = (!pt).(0) ; fin= (!pt).(1) ; taille= dist ((!pt.(0)).x,(!pt.(0)).y) ((!pt.(1)).x,(!pt.(1)).y)};{debut = (!pt).(1) ; fin= (!pt).(2) ; taille= dist ((!pt.(1)).x,(!pt.(1)).y) ((!pt.(2)).x,(!pt.(2)).y)} ;{debut = (!pt).(2) ; fin= (!pt).(3) ; taille= dist ((!pt.(2)).x,(!pt.(2)).y) ((!pt.(3)).x,(!pt.(3)).y)}|]in
while true do
	for i=0 to (length !pt)-1 do
		vx:= !pt.(i).x -. !pt.(i).oldx ;
		vy:= !pt.(i).y -. !pt.(i).oldy ;
		!pt.(i) <- {x= !pt.(i).x +. !vx; y= !pt.(i).y +. !vy -. 0.001; oldx= !pt.(i).x; oldy= !pt.(i).y  };
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
		moveto (int_of_float st.(i/2).debut.x) (int_of_float st.(i/2).debut.y);
		lineto (int_of_float st.(i/2).fin.x) (int_of_float st.(i/2).fin.y);
		draw_image image ((int_of_float !pt.(i).x)-25) ((int_of_float !pt.(i).y)-25) ;
		
		if (i != 0) && (i != (length !pt)-1) then
		begin st.(i-1).fin<- !pt.(i); st.(i).debut<- !pt.(i) end;
		if i=0 then st.(0).debut<- !pt.(i)
		else st.((length st)-1).fin<- !pt.(i);
	done;
	synchronize ();
	clear_graph();
	update st;
	
	
done;;
