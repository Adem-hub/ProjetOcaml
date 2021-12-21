#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open Array;;
open_graph " 800x600";;

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

type point   = {mutable x: float;  mutable y: float; mutable oldx:float; mutable oldy:float};;
type stick = {mutable debut: point;  mutable fin: point; mutable taille:float};;
type 'a maliste = {mutable size:int; mutable tab:  'a array};;
let dist (x1,y1) (x2,y2) = sqrt((x1-.x2)**2. +. (y1-.y2)**2.);;

let update lien ta temps= 
	for i=0 to ta do 
		let dx= lien.(i).fin.x -. lien.(i).debut.x 
		and dy= lien.(i).fin.y -. lien.(i).debut.y in
		let distance = sqrt( dx**2. +. dy**2.) in
		let diff = lien.(i).taille -. distance in 
		let pourcent = diff /. distance /.2. in
		let decX = dx *. pourcent
		and decY = dy *. pourcent in
			if i!=0   then begin
			lien.(i).debut.x <- (lien.(i).debut.x) -. decX;
			lien.(i).debut.y <- (lien.(i).debut.y) -. decY end;
			if i!=ta || Unix.gettimeofday()-.temps>=5.  then begin 
			lien.(i).fin.x   <- (lien.(i).fin.x) +. decX;
			lien.(i).fin.y   <- (lien.(i).fin.y) +. decY end;
		done;;

let ajoute liste valeur = let prov= make (liste.size*2) valeur and taille=liste.size in
								  if length (liste.tab) > liste.size then begin
								  liste.tab.(taille)<-valeur;
								  liste.size<-liste.size+1;
								  end
								  else begin 
								  for i=0 to (liste.size)-1 do
										prov.(i)<- liste.tab.(i);
										done;
									liste.size<-liste.size+1;
									prov.(taille)<-valeur;
									liste.tab<-prov;
									end;;

let makepttab = let provtab = {size=1;  tab= [|{x = 300.; y = 400.;oldx=300.;oldy=400.}|]} in
							let point = provtab.tab.(0) in
							for i=1 to 10 do
								ajoute provtab {x=point.x-. float_of_int(10*i);
													 y=point.y;
													 oldx=point.oldx-. float_of_int(10*i);
													 oldy=point.oldy }
							done;provtab;;

let makesttab pttab len= let p = {size=1;  tab= [|{debut = (!pttab).(0) ; fin= (!pttab).(1) ; taille= dist ((!pttab.(0)).x,(!pttab.(0)).y) ((!pttab.(1)).x,(!pttab.(1)).y)}|]} in
									 for i=1 to len-1  do 
										ajoute p {debut = (!pttab).(i) ;
													 fin= (!pttab).(i+1) ;
													 taille= dist ((!pttab.(i)).x,(!pttab.(i)).y) ((!pttab.(i+1)).x,(!pttab.(i+1)).y)};
										done;p.tab;;

let pt = ref makepttab.tab;;
let st = makesttab pt 10;;
pt;;
st;;
auto_synchronize false;;
display_mode false;;
let vx = ref 0.
and vy = ref 0.
and ti = Unix.gettimeofday()
and cste_elastique =3
and g= 9.81
and dt = ref (Unix.gettimeofday())
and pt = ref makepttab.tab and tot = makepttab.size in
let st = makesttab pt tot in
		while true do
			if Unix.gettimeofday()-.(!dt)>1./.60. then begin
				for i = 0 to tot-1  do
					vx := (!pt.(i).x -. !pt.(i).oldx) ;
					vy := (!pt.(i).y -. !pt.(i).oldy) ;
					if (i != 0 && (i!=tot -1 || Unix.gettimeofday()-.ti>5. )) then begin
						!pt.(i) <- {x = !pt.(i).x +. !vx;
										y = !pt.(i).y +. !vy -. g*.0.01;
										oldx = !pt.(i).x;
										oldy = !pt.(i).y};
						end;
					if (i==tot-1) then begin
						draw_image image (int_of_float (!pt.(i).x-.25.)) (int_of_float (!pt.(i).y-.25.));
						if (Unix.gettimeofday()-.ti>=5.) then 
						!pt.(i) <- {x = !pt.(i).x +. !vx; y = !pt.(i).y +. !vy -. 0.1*.g; oldx = !pt.(i).x; oldy = !pt.(i).y};
						end;
					if i!=tot-1 then begin
					moveto (int_of_float st.(i).debut.x) (int_of_float st.(i).debut.y);
					lineto (int_of_float st.(i).fin.x) (int_of_float st.(i).fin.y);
					end;
					
					if (i != 0) && (i!= tot-1) then
						begin st.(i - 1).fin <- !pt.(i); st.(i).debut <- !pt.(i) end;
					if i = 0 then st.(0).debut <- !pt.(i)
					else if  i= tot-1 then  st.(tot-2).fin <- !pt.(i);
				done;
				synchronize ();
				clear_graph ();
				for i=0 to cste_elastique do 
					update st (tot-2) ti;
				done;
				dt:=Unix.gettimeofday();
			end;
		done;;
