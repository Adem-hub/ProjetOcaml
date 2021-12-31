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
type 'a tableau_dynamique = {size   : unit      -> int;
									  id     : int       -> 'a;
									  add    : 'a        -> unit;
									  remove : int       -> unit;
									  switch : int -> 'a -> unit} ;;
									  
let dist (x1,y1) (x2,y2) = sqrt((x1-.x2)**2. +. (y1-.y2)**2.);;








let update lien boule temps= 
	for  i = 0 to boule do 
		let dx = (lien.id i).fin.x -. (lien.id i).debut.x 
		and dy = (lien.id i).fin.y -. (lien.id i).debut.y in
		let distance = sqrt( dx**2. +. dy**2.) in
		let diff = (lien.id i).taille -. distance in 
		let pourcent = diff /. distance /.2. in
		let decX = dx *. pourcent
		and decY = dy *. pourcent in
			if i!=0  then 
				begin
				(lien.id i).debut.x <- (lien.id i).debut.x -. decX;
				(lien.id i).debut.y <- (lien.id i).debut.y -. decY
				end;
			if i!=boule || Unix.gettimeofday()-.temps>=5.  then
				begin 
				(lien.id i).fin.x   <- (lien.id i).fin.x +. decX;
				(lien.id i).fin.y   <- (lien.id i).fin.y +. decY 
				end;
		done;;
		
		
(* init pour le tableau dynamique : default ne sera pas dans le tableau *)
let make_tab default =
   let taille  = ref 0 
   and support = ref [|default|] in
	let ajoute valeur =
		if !taille <> Array.length (!support) then
			begin
			(!support).(!taille) <- valeur;
			end
		else
			begin
			let new_support = Array.make ((!taille) * 2) valeur in
			for i = 0 to ((!taille) - 1) do
				new_support.(i) <- (!support).(i);
			done;
			support := new_support;
			end;
		taille := !taille + 1;
	and supprime indice = 
		for i = indice to ((!taille) - 2) do
			(!support).(i) <- (!support).(i+1);
		done;
		taille := !taille -1;
	and change id valeur = 
		(!support).(id) <- valeur;
	in {size     = (fun () -> !taille);
		 id       = (fun i -> if i < !taille then !support.(i) else failwith "Index out of range");
		 add      = ajoute;
		 remove   = supprime;
		 switch   = change};;

let make_pts_tab valeur = let tab_pts = make_tab valeur in
								  let point = valeur in
								  for i=0 to 10 do
									tab_pts.add 	{x    =point.x-. float_of_int(20*i);
														 y    =point.y;
														 oldx =point.oldx-. float_of_int(20*i);
														 oldy =point.oldy }
								  done;tab_pts;;

let make_liens_tab pttab len= let tab_liens = make_tab {debut  = {x = 300.; y = 400.; oldx = 300.; oldy = 400.};
																	     fin    = {x = 290.; y = 400.; oldx = 290.; oldy = 400.};
																        taille = 10.} in
									   for i = 0 to len-2  do 
											tab_liens.add {debut  = (!pttab).id i ;
																fin    = (!pttab).id (i+1) ;
																taille = dist ((!pttab.id (i)).x ,(!pttab.id (i)).y) ((!pttab.id (i+1)).x ,(!pttab.id (i+1)).y)};
										done;tab_liens;;


auto_synchronize false;;
display_mode false;;

let vx     = ref 0.
and vy     = ref 0.
and t_init = Unix.gettimeofday()
and cste   = 3
and g      = 9.81
and bool   = ref false
and dt     = ref (Unix.gettimeofday())
and points = ref (make_pts_tab {x = 300.; y = 400.; oldx = 300.; oldy = 400.}) in
let total  = ref (!points.size()) in
let liens  = make_liens_tab points (!total) in
		while true do
			if Unix.gettimeofday()-.(!dt)>1./.60. then 
				begin
				for i = 0 to !total-1  do
					vx := (!points.id i).x -. (!points.id i).oldx ;
					vy := (!points.id i).y -. (!points.id i).oldy ;
					if (i <> 0 && (i <> !total -1 || Unix.gettimeofday()-.t_init>5. )) then begin
						(!points).switch i  ({x    = (!points.id i).x +. !vx;
													 y    = (!points.id i).y +. !vy -. g*.0.01;
													 oldx = (!points.id i).x;
													 oldy = (!points.id i).y});
				end;
				if (i = !total-1) then 
					begin
						draw_image image (int_of_float (!points.id i).x - 25) (int_of_float (!points.id i).y - 25);
						if (Unix.gettimeofday()-.t_init>=5.) then 
							(!points).switch i  ({x    = (!points.id i).x +. !vx;
														 y    = (!points.id i).y +. !vy -. g*.0.1;
														 oldx = (!points.id i).x;
														 oldy = (!points.id i).y});
					end;
				if i <> !total-1 then 
					begin
					moveto (int_of_float (liens.id i).debut.x) (int_of_float (liens.id i).debut.y);
					lineto (int_of_float (liens.id i).fin.x) (int_of_float (liens.id i).fin.y);
					end;
				
				if (i <> 0) && (i <> !total-1) then
					begin 
					(liens.id (i - 1)).fin <- (!points.id i);
					(liens.id i).debut     <- (!points.id i)
					end;
				if i = 0 then
					(liens.id 0).debut <- (!points.id i)
				else if  i= !total-1 then
					(liens.id (!total-2)).fin <- (!points.id i);
				done;
				
				synchronize ();
				clear_graph ();
				
				for i=0 to cste do 
					update liens (!total-2) t_init;
				done;
				dt:=Unix.gettimeofday();
			end;
			
			
		done;;
