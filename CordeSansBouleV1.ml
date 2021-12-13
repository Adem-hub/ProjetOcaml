#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open Array;;
open_graph " 800x600";;


type point   = {mutable x: float;  mutable y: float; mutable oldx:float; mutable oldy:float};;
type stick = {mutable debut: point;  mutable fin: point; mutable taille:float};;
type 'a maliste = {mutable taille:int; mutable tab:  'a array};;
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
			if i!=0 then begin
			lien.(i).debut.x <- (lien.(i).debut.x) -. decX;
			lien.(i).debut.y <- (lien.(i).debut.y) -. decY end;
			lien.(i).fin.x   <- (lien.(i).fin.x) +. decX;
			lien.(i).fin.y   <- (lien.(i).fin.y) +. decY;
		done;;

let ajoute liste valeur = let prov= make (liste.taille*2) valeur and taille=liste.taille in
								  if length (liste.tab) > liste.taille then begin
								  liste.tab.(taille)<-valeur;
								  liste.taille<-liste.taille+1;
								  end
								  else begin 
								  for i=0 to (liste.taille)-1 do
										prov.(i)<- liste.tab.(i);
										done;
									liste.taille<-liste.taille+1;
									prov.(taille)<-valeur;
									liste.tab<-prov;
									end;;

let makepttab = let provtab = {taille=1;  tab= [|{x = 200.; y = 400.;oldx=199.;oldy=399.}|]} in
							let point = provtab.tab.(0) in
							for i=1 to 40 do
								ajoute provtab {x=point.x;y=point.y-. float_of_int(5*i);oldx=point.oldx;oldy=point.oldy -. float_of_int(5*i)}
							done;provtab.tab;;

let makesttab pttab= let p = {taille=1;  tab= [|{debut = (!pttab).(0) ; fin= (!pttab).(1) ; taille= dist ((!pttab.(0)).x,(!pttab.(0)).y) ((!pttab.(1)).x,(!pttab.(1)).y)}|]} in
									 for i=1 to (length !pttab)-2  do 
										ajoute p {debut = (!pttab).(i) ; fin= (!pttab).(i+1) ; taille= dist ((!pttab.(i)).x,(!pttab.(i)).y) ((!pttab.(i+1)).x,(!pttab.(i+1)).y)};
										done;p.tab;;

auto_synchronize false;;
display_mode false;;
let vx     = ref 0.
and vy     = ref 0.
and pt = ref makepttab in
let st = makesttab pt in
while true do
	for i=0 to (length !pt)-1 do
		vx:= !pt.(i).x -. !pt.(i).oldx ;
		vy:= !pt.(i).y -. !pt.(i).oldy ;
		if i != 0 then
		!pt.(i) <- {x= !pt.(i).x +. !vx; y= !pt.(i).y +. !vy -. 0.01; oldx= !pt.(i).x; oldy= !pt.(i).y  };
		
		moveto (int_of_float st.(i/2).debut.x) (int_of_float st.(i/2).debut.y);
		lineto (int_of_float st.(i/2).fin.x) (int_of_float st.(i/2).fin.y);
		draw_circle (int_of_float !pt.(i).x) (int_of_float !pt.(i).y) 1;
		fill_circle (int_of_float !pt.(i).x) (int_of_float !pt.(i).y) 1;
		
		if (i != 0) && (i != (length !pt)-1) then
		begin st.(i-1).fin<- !pt.(i); st.(i).debut<- !pt.(i) end;
		if i=0 then st.(0).debut<- !pt.(i)
		else st.((length st)-1).fin<- !pt.(i);
	done;
	synchronize ();
	clear_graph();
	update st;
	
	
done;;
