(* Import *)

#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open Array;;


(* Constantes *)

(* lien du fichier à modifier *)
let working_path = "C:\\Users\\admin\\Desktop\\WinCaml\\";;
(* physique *)
let g           = -9.81;;
let friction    = 0.999;;
(* technique *)
let bounce      = 3;;
let lien_unit   = 10.;;
let sensibility = 2.;;
(* graphique *)
let color_lien1 = rgb 125 80 55;;
let color_lien2 = rgb 220 150 90;;
let hauteur     = 1000;;
let largeur     = 800;;
let marcus_x    = 160;;
let marcus_y    = 70;;

(* Types *)

type img   = {data   : image;
			     height : int;
				  width  : int};;
					
type gif   = {imgs   : img array;
				  frames : int};;

type point = {mutable x      : float;
              mutable y      : float;
              mutable oldx   : float;
              mutable oldy   : float;
              mutable pinned : bool};;

type stick = {mutable debut    : point;
              mutable fin      : point};;

type corde = {x_c          : float;
				  y_c          : float;
				  len          : int;
				  mutable used : bool};;

type 'a tableau_dynamique = {size   : unit      -> int;
									  id     : int       -> 'a;
									  add    : 'a        -> unit;
									  remove : int       -> unit};;

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
	in {size     = (fun () -> !taille);
		 id       = (fun i -> if i < !taille then !support.(i) else failwith "Index out of range");
		 add      = ajoute;
		 remove   = supprime};;


(* Fonctions outils *)

let distance x1 y1 x2 y2 = sqrt ((x1 -. x2)**2. +. (y1 -. y2)**2.);;

(* renvoie l'id de bout touché et -1 si aucun n'est touché*)
let contact_lien liens =
	let x_mouse, y_mouse = mouse_pos () in 
	let x = float_of_int x_mouse
	and y = float_of_int y_mouse in
	let id = ref (-1) in
	for i = 0 to liens.size () - 1 do
		let xi = if (liens.id (i)).debut.x > (liens.id (i)).fin.x
					then (liens.id (i)).debut.x +. sensibility
					else (liens.id (i)).debut.x -. sensibility
		and yi = if (liens.id (i)).debut.y > (liens.id (i)).fin.y
					then (liens.id (i)).debut.y +. sensibility
					else (liens.id (i)).debut.y -. sensibility
		and xf = if (liens.id (i)).debut.x < (liens.id (i)).fin.x
					then (liens.id (i)).fin.x +. sensibility
					else (liens.id (i)).fin.x -. sensibility
		and yf = if (liens.id (i)).debut.y < (liens.id (i)).fin.y
					then (liens.id (i)).fin.y +. sensibility
					else (liens.id (i)).fin.y -. sensibility in
		if (xi -. x) *. (xf -. x) <= 0. && (yi -. y) *. (yf -. y) <= 0. then
			id := i;
	done;
	!id;;

let out_screen x y =
	let test = ref false in
	if int_of_float y > hauteur + 100 then
		test := true;
	if int_of_float y < -100 then
		test := true;
	if int_of_float x > largeur + 100 then
		test := true;
	if int_of_float x < -100 then
		test := true;
	!test;;

let touch_marcus x y =
	let x1 = (float_of_int marcus_x) -. 110.
	and y1 = (float_of_int marcus_y) -. 110.
	and x2 = (float_of_int marcus_x) +. 110.
	and y2 = (float_of_int marcus_y) +. 110. in
	if (x1 -. x) *. (x2 -. x) <= 0. && (y1 -. y) *. (y2 -. y) <= 0. then
		true
	else
		false;;


(* Fonction d'update *)

let update_pts points =
	for i = 0 to points.size () - 1 do
		if (points.id i).pinned = false then (* ajouter les paramètrages temporels *)
			begin
			let vx = ((points.id i).x -. (points.id i).oldx) *. friction
			and vy = ((points.id i).y -. (points.id i).oldy) *. friction in
			(points.id i).oldx <- (points.id i).x;
			(points.id i).oldy <- (points.id i).y;
			(points.id i).x    <- (points.id i).x +. vx;
			(points.id i).y    <- (points.id i).y +. vy +. g *. 0.01;
			end;
	done;;

let update_liens liens= 
	for  i = 0 to liens.size () - 1 do
		let dx         = (liens.id i).fin.x -. (liens.id i).debut.x 
		and dy         = (liens.id i).fin.y -. (liens.id i).debut.y in
		let distance   = sqrt (dx**2. +. dy**2.) in
		let difference = lien_unit -. distance in 
		let pourcent   = difference /. distance /. 2. in
		let offset_x   = dx *. pourcent
		and offset_y   = dy *. pourcent in
			if (liens.id i).debut.pinned = false then
				begin
				(liens.id i).debut.x <- (liens.id i).debut.x -. offset_x;
				(liens.id i).debut.y <- (liens.id i).debut.y -. offset_y;
				end;
			if (liens.id i).fin.pinned = false then
				begin
				(liens.id i).fin.x   <- (liens.id i).fin.x +. offset_x;
				(liens.id i).fin.y   <- (liens.id i).fin.y +. offset_y;
				end;
		done;;

let check_rope points liens ropes =
	for i = 0 to ropes.size () - 1 do
		if (ropes.id i).used = false then
			begin
			let x_ball   = (points.id 0).x
			and y_ball   = (points.id 0).y
			and x_centre = (ropes.id i).x_c
			and y_centre = (ropes.id i).y_c in
			if float_of_int ((ropes.id i).len) *. lien_unit >= abs_float (distance x_ball y_ball x_centre y_centre) then
				begin
				let x_step = (x_ball -. x_centre) /. float_of_int ((ropes.id i).len)
				and y_step = (y_ball -. y_centre) /. float_of_int ((ropes.id i).len) 
				and indice = points.size () in
				for j = 0 to (ropes.id i).len - 1 do 
					points.add {x      = x_centre +. x_step *. float_of_int j;
									y      = y_centre +. y_step *. float_of_int j;
									oldx   = x_centre +. x_step *. float_of_int j;
									oldy   = y_centre +. y_step *. float_of_int j;
									pinned = false};
				done;
				(points.id indice).pinned <- true;
				for j = 0 to (ropes.id i).len - 2 do
					liens.add {debut = points.id (indice + j);
								  fin   = points.id (indice + j + 1)};
				done;
				liens.add {debut = points.id (indice + (ropes.id i).len - 1);
							  fin   = points.id 0};
				(ropes.id i).used <- true;
				end;
			end;
	done;;


(* Fonctions d'affichage *)

let chargement pourcentage fichier = 
	let couleur = rgb 25 85 110 in
	set_color couleur;
	fill_rect 0 0 largeur hauteur;
	set_color white;
	set_text_size 12;
	moveto 10 30;
	draw_string fichier;
	set_text_size 40;
	moveto 525 30;
	draw_string "Loading...";
	set_text_size 14;
	moveto 110 800;
	draw_string "This story is about Marcus, a small and hungry dinosaur. Marcus was";
	moveto 110 750;
	draw_string "walking in the forest and lost himself. He isn't so worried about";
	moveto 110 700;
	draw_string "finding his way home. Perhaps, as a dinosaur, Marcus got a fine sense";
	moveto 110 650;
	draw_string "of smell. But something still bothers him; he hasn't eaten in a while.";
	moveto 110 600;
	draw_string "He starts looking around and arrives near a montain. There, Marcus was";
	moveto 110 550;
	draw_string "surprised to see a wood escalator. Were escalators growing in trees,";
	moveto 110 500;
	draw_string "perhaps not it was a sign of life. Marcus uses it to climb the montane";
	moveto 110 450;
	draw_string "and see the beautiful old house where you live. He knocks on the door.";
	moveto 110 400;
	draw_string "You open and see this desperate dinosaur. You grab your magic wand, which";
	moveto 110 350;
	draw_string "can cut any rope at any distance, and help him get a pleasant dinner.";
	let charge = int_of_float (float_of_int largeur *. pourcentage) in
	fill_rect 0 0 charge 20;
	synchronize ();;

let print_liens liens =
	set_color color_lien1;
	set_line_width 3;
	for i = 0 to liens.size () - 1 do
		if i mod 2 = 1 then
			set_color color_lien2
		else
			set_color color_lien1;
		moveto (int_of_float (liens.id i).debut.x) (int_of_float (liens.id i).debut.y);
		lineto (int_of_float (liens.id i).fin.x) (int_of_float (liens.id i).fin.y);
	done;
	set_line_width 1;;

let print_ball ball x y =
	let x_reel = int_of_float (x -. 30.)
	and y_reel = int_of_float (y -. 30.) in
	draw_image ball.data x_reel y_reel;;

let print_ropes ropes point =
	for i = 0 to ropes.size () - 1 do
		let x     = int_of_float ((ropes.id i).x_c)
		and y     = int_of_float ((ropes.id i).y_c)
		and rayon = ((int_of_float lien_unit) * (ropes.id i).len) in
		set_color black;
		draw_image point.data (x - 5) (y - 5);
		if (ropes.id i).used = false then
			begin
			set_color red;
			draw_circle x y rayon;
			end;
	done;;

let affiche_marcus char id mode =
	let frame = !id mod (char.(!mode)).frames
	and x     = if !mode = 2 then marcus_x - 15 else marcus_x
	and y     = if !mode = 2 then marcus_y - 20 else marcus_y in
	draw_image (char.(!mode)).imgs.(frame).data x y;;



(* Fonctions d'importation *)

(* nécessite la fenêtre utilisée déjà ouverte *)
let load_brc relative_link =
	let file  = open_in (working_path ^ relative_link ^ ".brc") in
	let width, height = Scanf.sscanf (input_line file) "%dx%d" (fun a b -> (a, b)) in
	let pre_image = Array.make_matrix width height transp in
		for i = 0 to width - 1 do
			for j = 0 to height - 1 do
				let line = input_line file in
				let r, g, b, a = Scanf.sscanf line "%d %d %d %d" (fun a b c d -> (a, b, c, d)) in
				if a < 125 then
					begin
					pre_image.(i).(j) <- (transp);
					end
				else
					begin
					pre_image.(i).(j) <- (rgb r g b);
					end;
			done;
			chargement ((float_of_int i) /. (float_of_int (width-1))) (relative_link ^ ".brc");
  		done;
  		{data = make_image pre_image; height = height; width = width};;

(* nécessite la fenêtre utilisée déjà ouverte *)
let load_brc_set frames relative_link =
	let empty_image = {data = make_image [|[|-1|]|]; height = 0; width = 0} in
	let gif_image   = {imgs   = Array.make frames empty_image;
						    frames = frames} in
	for i = 1 to frames do
		let path      = relative_link ^ ("-" ^ (string_of_int i)) in
		gif_image.imgs.(i-1) <- load_brc path;
	done;
	gif_image;;


(* Main *)

let main =
	(* ouverture de la fenêtre *)
	let dimension = (string_of_int largeur) ^ "x" ^ (string_of_int hauteur) in
	open_graph dimension;
	auto_synchronize false;
	display_mode false;
	(* écran de chargement *)
	chargement 0. " ";
	let char1  = load_brc_set 5 "Images\\Dragon\\Win"
	and char2  = load_brc_set 5 "Images\\Dragon\\Waiting"
	and char3  = load_brc_set 4 "Images\\Dragon\\Loose"
	and slice  = load_brc_set 7 "Images\\Effects\\Slice"
	and ball   = load_brc "Images\\Ball\\Ball"
	and point  = load_brc "Images\\Point\\Point"
	and back   = load_brc "Images\\Decor\\Back"
	and front  = load_brc "Images\\Decor\\Front" in
	let marcus = [|char1; char2; char3|]
	and time   = ref (Unix.gettimeofday ())
	and dt     = ref (Unix.gettimeofday ())
	and delay  = ref (Unix.gettimeofday ())
	and niveau = ref 1 in
		(* boucle du jeu *)
		while !niveau < 2 do
			let  points = make_tab {x = 0.; y = 0.; oldx = 0.; oldy = 0.; pinned = false}
			and liens  = make_tab {debut = {x = 0.; y = 0.; oldx = 0.; oldy = 0.; pinned = false};
								        fin   = {x = 0.; y = 0.; oldx = 0.; oldy = 0.; pinned = false}}
			and ropes  = make_tab {x_c = 0.; y_c = 0.; len = 0; used = false}
			and mode   = ref 1
			and id     = ref 0 
			and en_jeu = ref true in
			points.add {x = 300.; y = 800.; oldx = 300.; oldy = 800.; pinned = false};
			ropes.add {x_c = 250.; y_c = 500.; len = 30; used = false};
			ropes.add {x_c = 400.; y_c = 600.; len = 20; used = false};
			while !en_jeu do
				if (Unix.gettimeofday ()) -. !dt > 0.01 then
					begin
					dt := Unix.gettimeofday();
					(* calcules *)
					check_rope points liens ropes;
					update_pts points;
					for i = 0 to bounce do 
						update_liens liens;
					done;
					if (Unix.gettimeofday ()) -. !time > 0.10 then
						begin
						id   := (!id + 1) mod 140; (* PPCM de tous les animations *)
						time := Unix.gettimeofday ();
						end;
					(* affichage *)
					draw_image back.data 0 0;
					affiche_marcus marcus id mode;
					print_ropes ropes point;
					print_liens liens;
					print_ball ball (points.id 0).x (points.id 0).y;
					draw_image front.data 0 0;
					if button_down () then
							begin
							let frame = !id mod slice.frames
							and x_mouse, y_mouse = mouse_pos () in
							draw_image slice.imgs.(frame).data (x_mouse - 50) (y_mouse - 50);
							end;
					synchronize ();
					clear_graph ();
					end;
					(* checks *)
					if out_screen (points.id 0).x (points.id 0).y then
						begin
						mode   := 2;
						en_jeu := false;
						end;
					if touch_marcus (points.id 0).x (points.id 0).y then
						begin
						mode   := 0;
						en_jeu := false;
						incr niveau;
						end;
					if button_down() && Unix.gettimeofday () -. (!delay) > 0.25 then
						begin
						let indice = contact_lien liens in
						if indice <> -1 then
							begin
							liens.remove indice;
							delay := Unix.gettimeofday ();
							end;
						end;
			done;
			if !mode = 0 then
				begin
				set_text_size 30;
				set_color black;
				moveto 357 505;
				draw_string "Win !";
				set_color green;
				moveto 355 500;
				draw_string "Win !";
				end
			else
				begin
				set_text_size 30;
				set_color black;
				moveto 337 505;
				draw_string "Loose !";
				set_color red;
				moveto 335 500;
				draw_string "Loose !";
				end;
			synchronize ();
			clear_graph ();
		done;;