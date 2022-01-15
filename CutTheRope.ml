(* Import *)

#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open Array;;
open Sys;;


(* Constantes *)

(* lien du fichier: à modifier *)
let working_path = "C:\\Users\\abenr\\OneDrive\\Bureau\\CutTheRope\\";;

(* technique *)
let bounce       = 40;;
let lien_unit    = 5.;;
let sensibility  = 10.;;
let delta_t      = 0.01;;
let delta2_t     = delta_t *. delta_t;;
let scale        = 280.;;

(* physique *)
let pi           = acos (-1.);;
let g            = -9.81;;
let visco_air    = 1.85 *. 0.00001;;
let masse_ball   = 0.200;;
let masse_lien   = 0.02;;
let k_ball       = (1. /. masse_ball) *. 6. *. (30. /. scale) *. pi *. visco_air;;
let k_corde      = (1. /. masse_lien) *. 6. *. (1.5 /. scale) *. pi *. visco_air;;

(* graphique *)
let color_lien1  = rgb 125 80 55;;
let color_lien2  = rgb 220 150 90;;
let hauteur      = 1000;;
let largeur      = 800;;


(* Variables globales *)

(* mini-jeu *)
let x_dino   = ref 100;;
let x_burger = ref 500;;
let dir      = ref 1;;
let score    = ref 0;;

(* ajustement si non windows *)
let not_win32_dx = if Sys.win32 then ref 0 else ref 10;;


(* Types *)

type img    = {data   : image;
               height : int;
               width  : int};;
	       
type gif    = {imgs   : img array;
               frames : int};;
	       
type point  = {mutable x      : float;
               mutable y      : float;
               mutable oldx   : float;
               mutable oldy   : float;
               mutable pinned : bool};;
	       
type stick  = {mutable debut    : point;
               mutable fin      : point};;
	       
type rope   = {x_c          : float;
               y_c          : float;
               len          : int;
               mutable used : bool};;
	       
type pick   = {xi : int;
               yi : int;
               xf : int;
               yf : int};;
	       
type 'a tableau_dynamique = {size   : unit      -> int;
                             id     : int       -> 'a;
                             add    : 'a        -> unit;
                             remove : int       -> unit};;
			     
(* initialisation pour le tableau dynamique : créer un tableau de taille nulle typé grâce à 'default'
note: default ne sera pas dans le tableau *)
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
		taille := !taille - 1;
	in {size     = (fun () -> !taille);
		 id       = (fun i -> if i < !taille then !support.(i) else failwith "Index out of range");
		 add      = ajoute;
		 remove   = supprime};;
type niveau = {points : point tableau_dynamique;
               liens  : stick tableau_dynamique;
               ropes  : rope tableau_dynamique;
               picks  : pick tableau_dynamique};;
	       
	       
(* Fonctions outils / de vérification *)

let distance x1 y1 x2 y2 = sqrt ((x1 -. x2)**2. +. (y1 -. y2)**2.);;

(* fonction qui compte le numbre de niveau dans les fichiers *)
let get_levels () =
	let level  = ref 0
	and x      = ref 45
	and y      = ref 650 
	and tab    = ref (make_tab (0,0)) in
	let levels = make_tab (!tab) in
		while (Sys.file_exists (working_path ^ "\\Niveau\\Niveau-" ^ (string_of_int !level) ^ ".niv")) do
			(!tab).add (!x, !y);
			x := !x+ 180;
			if !x >= 720 then begin
				x := 45;
				y := !y - 170
				end;
			if !y <= 100 then begin
				y := 650;
				levels.add (!tab);
				tab := make_tab (0,0);
				end;
			incr level;
			done;
			levels.add (!tab);
			levels;;
			
(* la fonction renvoie l'id du lien "touché" et -1 si aucun lien n'est "touché"
note: "touché" correspond à la souris dans le carré décrit par les coordonnées des extémités du liens à une sensibilité près*)
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
	
(* fonction qui vérifie si le hamburger est sortie de l'écran
note: la sortie de l'écran est considéré plus loin pour que la balle sorte réellement de l'écran, ie froller le bord est accepté *)
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
	
(* fonction qui vérifie si le hamburger est sur Marcus (le dinosaure)
note: on vérifie si le hamburger est dans un carré centré sur marcus sur lui qui fait ça taille et le diamètre de la balle de coté *)
let touch_marcus x y =
	if (180. -. x) *. (290. -. x) <= 0. && (90. -. y) *. (200. -. y) <= 0. then
		true
	else
		false;;
(* fonction qui vérifie si la ball est en collision avec une ligne de piques *)
let picks_collision picks x y = 
    let touche = ref false
    and i = ref 0 in
    while !i < picks.size() && !touche = false do 
			let xi = float_of_int (picks.id !i).xi
			and xf = float_of_int (picks.id !i).xf
			and yi = float_of_int (picks.id !i).yi
			and yf = float_of_int (picks.id !i).yf in 
			if xi = xf && abs_float(x -. xi) < 35. && (y -. yi) *. (y -. yf) <= 0. then 
				begin 
				touche := true;
				end;
			if yi = yf && abs_float(y -. yi) < 35. && (x -. xi) *. (x -. xf) <= 0. then 
				begin 
				touche := true;
				end;
			incr i;
      done;
      !touche;;
      
      
(* Fonction d'update *)
(* fonction qui calcules les coordonnées des points à t + delta_t *)

let update_pts points =
	for i = 0 to points.size () - 1 do
		if (points.id i).pinned = false then
			begin
			let coef = if i = 0 then k_ball else k_corde in (* Le coefficicent de frottement diffère si on parle de la balle *)
			let vx    = ((points.id i).x -. (points.id i).oldx)
			and vy    = ((points.id i).y -. (points.id i).oldy) in
			(points.id i).oldx <- (points.id i).x;
			(points.id i).oldy <- (points.id i).y;
			(points.id i).x    <- (points.id i).x +. vx *. (1. -. coef *. delta2_t);
			(points.id i).y    <- (points.id i).y +. vy *. (1. -. coef *. delta2_t) +. g *. delta2_t *. scale;
			end;
	done;;
	
(* fonction qui actualise les liens afin qu'il garde la bonne distance: lien_unit *)
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
		
(* fonction vérifiant si une corde non utilisée doit-être activée, si oui la corde sera alors créée
note: dans update car actualise la liste des cordes *)
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

(* fonction qui dessine le dinosaure du mini-jeu *)
let draw_dino x y dir =
	set_color (rgb 16 117 95);
	fill_circle (x - dir * 40) (y + 20) 5;
	fill_circle (x - dir * 40) (y + 30) 5;
	fill_circle (x - dir * 40) (y + 40) 5;
	fill_circle (x - dir * 40) (y + 50) 5;
	fill_circle (x - dir * 40) (y + 60) 5;
	fill_circle (x - dir * 40) (y + 70) 5;
	fill_circle (x - dir * 40) (y + 80) 5;
	fill_circle (x - dir * 40) (y + 90) 5;
	fill_circle (x - dir * 30) (y + 100) 10;
	fill_circle (x - dir * 20) (y + 110) 10;
	fill_circle (x - dir * 10) (y + 110) 15;
	fill_circle x (y + 110) 20;
	set_color (rgb 91 199 176);
	fill_poly [|((x - dir * 40), y);
                    ((x - dir * 60), y);
                    ((x - dir * 40), (y + 20))|];
	fill_rect (x -40) y 80 80;
	fill_rect (x - 25) (y - 20) 10 20;
	fill_rect (x + 15) (y - 20) 10 20;
	fill_circle x (y + 80) 40;
	set_color black;
	fill_circle (x + dir * 20) (y + 80) 5;
	if dir = 1 then
		fill_rect (x + 10) (y + 60) 30 2
	else
		fill_rect (x - 40) (y + 60) 30 2;;
		
(* fonction qui dessine le hamburger du mini-jeu *)
let draw_burger x y =
	set_color (rgb 173 128 55);
	fill_ellipse x (y + 5) 30 15;
	fill_ellipse x (y - 5) 30 15;
	set_color (rgb 84 53 3);
	fill_rect (x - 30) (y - 5) 60 10;
	fill_circle (x - 30) y 5;
	fill_circle (x + 30) y 5;;
	
(* fonction qui gère le mini jeu
note: elle ne fait pas d'affichage mais est nécessaire à chargement d'où sa présence ici *)
let mini_jeu () =
	if key_pressed () then
		begin
		let key = read_key () in
		if key = 'a' && !x_dino > 50 then
			begin
			x_dino := !x_dino - 10;
			dir    := -1;
			end;
		if key = 'e' && !x_dino < 750 then
			begin
			x_dino := !x_dino + 10;
			dir    := 1;
			end;
		end;
	if abs(!x_dino - !x_burger) < 70 then
		begin
		x_burger := (Random.int 600) + 100;
		incr score;
		end;
	draw_dino !x_dino 150 !dir;
	draw_burger !x_burger 150;;
	
(* fonction qui affiche l'écran de chargement *)
let chargement pourcentage fichier = 
	let couleur = rgb 25 85 110 in
	set_color couleur;
	fill_rect 0 0 largeur hauteur;
	set_color white;
	if Sys.win32 then
		begin
		set_text_size 12;
		end
	else
		begin
		not_win32_dx := 36;
		end;
	moveto 10 30;
	draw_string fichier;
	if Sys.win32 then
		begin
		set_text_size 40;
		end
	else
		begin
		not_win32_dx := 120;
		end;
	moveto (525 + !not_win32_dx) 30;
	draw_string "Loading...";
	if Sys.win32 then
		begin
		set_text_size 14;
		end
	else
		begin
		not_win32_dx := 42;
		end;
	moveto (110 + !not_win32_dx) 800;
	draw_string "This story is about Marcus, a small and hungry dinosaur. Marcus was";
	moveto (110 + !not_win32_dx) 750;
	draw_string "walking in the forest and lost himself. He isn't so worried about";
	moveto (110 + !not_win32_dx) 700;
	draw_string "finding his way home. Perhaps, as a dinosaur, Marcus got a fine sense";
	moveto (110 + !not_win32_dx) 650;
	draw_string "of smell. But something still bothers him; he hasn't eaten in a while.";
	moveto (110 + !not_win32_dx) 600;
	draw_string "He starts looking around and arrives near a montain. There, Marcus was";
	moveto (110 + !not_win32_dx) 550;
	draw_string "surprised to see a wood escalator. Were escalators growing in trees,";
	moveto (110 + !not_win32_dx) 500;
	draw_string "perhaps not it was a sign of life. Marcus uses it to climb the montane";
	moveto (110 + !not_win32_dx) 450;
	draw_string "and see the beautiful old house where you live. He knocks on the door.";
	moveto (110 + !not_win32_dx) 400;
	draw_string "You open and see this desperate dinosaur. You grab your magic wand, which";
	moveto (110 + !not_win32_dx) 350;
	draw_string "can cut any rope at any distance, and help him get a pleasant dinner.";
	moveto (260 + !not_win32_dx) 300;
	draw_string "a to go left | e to go right";
	let charge = int_of_float (float_of_int largeur *. pourcentage)
	and scor = "Score : " ^ (string_of_int(!score)) in
	fill_rect 0 0 charge 20;
	if Sys.win32 then
		begin
		set_text_size 30;
		end
	else
		begin
		not_win32_dx := 90;
		end;
	moveto (600 + !not_win32_dx) 940;
	set_color white;
	draw_string scor;
	mini_jeu ();
	synchronize ();;
	
(* fonction qui affiche les liens des cordes
note: en utilisant un modulo pour la couleur lors de retrait de 1 liens les couleurs peuvent alterner,
cependant cet effet est peu pérceptible et ne gène pas au déroulement de la partie, aussi il est ignoré *)
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
	
(* fonction qui affiche le hamburger (en jeu) *)
let print_ball ball x y =
	let x_reel = int_of_float (x -. 30.)
	and y_reel = int_of_float (y -. 30.) in
	draw_image ball.data x_reel y_reel;;
	
(* fonction qui affiche les points d'attache des cordes ainsi que les cercles de détection *)
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
	
(* fonction qui affiche les piques en prenant compte du sens *)
let draw_picks picks imgs = 
	for i = 0 to (picks.size() - 1) do
		if (picks.id i).xi = (picks.id i).xf then
			begin
			let dist = abs ( (picks.id i).yi - (picks.id i).yf )
			and j = ref 0
			and x = (picks.id i).xi
			and y = (picks.id i).yi in
				for i = 0 to dist/10 - 1 do
					draw_image imgs.(1).data x (y + !j*10);
					incr j;
				done;
			end
		else
			begin
			let dist = abs ( (picks.id i).xi - (picks.id i).xf )
			and j = ref 0
			and x = (picks.id i).xi
			and y = (picks.id i).yi in
				for i = 0 to dist/10 - 1 do
					draw_image imgs.(0).data (x + !j*10) y;
					incr j;
				done;
			end;
	done;;
	
(* fonction d'affichage du menu *)
let display_levels levels page bg l_arrow r_arrow frame =
	draw_image bg.data 0 0;
	let tab    = (levels.id page) in
	for i = 0 to tab.size() - 1 do
		let x, y   = (tab.id i) 
		and numero = string_of_int(i + page*16) in
			draw_image frame.data x y;
			if Sys.win32 then
				begin
				set_text_size 60;
				set_font "Times";
				end;
			if i + page * 16 <10 then
				begin
				if Sys.win32 then 
					begin
					moveto (x+62) (y+45);
					end
				else 
					begin
					moveto (x+70) (y+72);
					end;
					draw_string numero;
				end
			else
				begin
				if Sys.win32 then 
					begin
					moveto (x+52) (y+45);
					end
				else 
					begin
					moveto (x+65) (y+72);
					end;
					draw_string numero;
				end;
			set_color (rgb 198 123 0);
		done;
		draw_image l_arrow.data 190 30 ;
		draw_image r_arrow.data 530 30 ;;
		
		
(* Fonctions d'importation *)

(* fonction qui permet d'importer un image au format .brc (cf. documentation du projet) et en fait un type image
note: nécessite la fenêtre utilisée déjà ouverte pour la fonction make_image de Graphics *)
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
		close_in file;
  		{data = make_image pre_image; height = height; width = width};;
		
(* fonction qui permet d'importer un gif sous forme de plusiseurs .brc (cf. documentation du projet) et en fait un type gif
note: nécessite la fenêtre utilisée déjà ouverte pour la fonction load_brc *)
let load_brc_set frames relative_link =
	let empty_image = {data = make_image [|[|-1|]|]; height = 0; width = 0} in
	let gif_image   = {imgs   = Array.make frames empty_image;
	                   frames = frames} in
	for i = 1 to frames do
		let path      = relative_link ^ ("-" ^ (string_of_int i)) in
		gif_image.imgs.(i-1) <- load_brc path;
	done;
	gif_image;;
	
(* fonction qui import un niveau de format .niv (cf. documentation du projet / Readme creation level) *)
let load_level id =
	let file   = open_in (working_path ^ "\\Niveau\\Niveau-" ^ (string_of_int id) ^ ".niv")
	and points = make_tab {x = 0.; y = 0.; oldx = 0.; oldy = 0.; pinned = false}
	and liens  = make_tab {debut = {x = 0.; y = 0.; oldx = 0.; oldy = 0.; pinned = false};
	                       fin   = {x = 0.; y = 0.; oldx = 0.; oldy = 0.; pinned = false}}
	and ropes  = make_tab {x_c = 0.; y_c = 0.; len = 0; used = false}
	and picks  = make_tab {xi = 0; yi = 0; xf = 0; yf = 0} in
		try
			while true do
				let line = input_line file in
				let check = Scanf.sscanf line "%s" (fun a -> a) in
				if check = "b" then
					begin
					let x, y = Scanf.sscanf line "b %f %f" (fun a b -> (a, b)) in
					points.add {x = x; y = y; oldx = x; oldy = y; pinned = false};
					end;
				if check = "c" then
					begin
					let x, y, l = Scanf.sscanf line "c %f %f %d" (fun a b c -> (a, b, c)) in
					ropes.add {x_c = x; y_c = y; len = l; used = false};
					end;
				if check = "p" then
					begin
					let xi, yi, dir, l = Scanf.sscanf line "p %d %d %d %d" (fun a b c d -> (a, b, c, d)) in
					let xf = if dir = 0
						 then xi
						 else xi + 10 * l
					and yf = if dir = 1
						 then yi
						 else yi + 10 * l in
					picks.add {xi = xi; yi = yi; xf = xf; yf = yf};
					end;
			done;
			{points = points; liens  = liens; ropes  = ropes; picks  = picks};
		with End_of_file ->
         		close_in file;
          		{points = points; liens  = liens; ropes  = ropes; picks  = picks};;


(* Fonction de gestion du jeu *)

(* fonction de détection de clique sur les niveaux dans le menu *)
let detect_level levels page = 
	let tab = (levels.id page)
	and lvl = ref (-1) in
	for i = 0 to tab.size() - 1 do
		let x, y   = (tab.id i)
		and px, py = mouse_pos() in
		if (px >= x && px <= x + 150 && py >= y && py <= y + 150) && button_down () then
			begin
			lvl := i + 16 * page;
			end;
	done;
	!lvl;;
	
(* fonction qui gère le changement de page *)
let change_page page booleen max = 
	let px, py = mouse_pos() 
	and coords = [|190;530|]  in 
	for i = 0 to 1 do 
		if (px >= coords.(i) && px <= coords.(i) + 74 && py >= 30 && py <= 60) then
		begin
		if button_down () && not (!booleen) then 
			begin
			booleen := true;
			if i = 0 && !page > 0 then 
				decr page;
			if i = 1 && !page < max then
				incr page;
							
			end;
		if not(button_down ()) && (!booleen) then
			booleen := false;
		end;
	done;;
	
(* fonction qui gère le menu *)
let niveaux bg frame r_arrow l_arrow = 
	let levels_tab    = get_levels ()
	and page          = ref 0 
	and level         = ref (-1) 
	and time          = ref (Sys.time())
	and booleen       = ref false in
		while !level = (-1) do
			display_levels levels_tab (!page) bg l_arrow r_arrow frame;
			if Sys.time() -. !time > 1. then
				begin
				level := detect_level levels_tab (!page);
				end;
			change_page page booleen (levels_tab.size() - 1);
			synchronize ();
		done;
		!level;;
		
(* fonction qui gère le fonctionnement pendant une partie *)
let partie niveau marcus ball slice point spikes back front =
	if Sys.win32 then
		begin
		set_font "Liberation-18:antialias=false";
		end;
	let is_win       = ref false
	and en_jeu       = ref true
	and marcus_frame = ref 0
	and slice_frame  = ref 0
	and frame_time   = ref (Sys.time ())
	and main_time    = ref (Sys.time ())
	and delay        = ref (Sys.time ())in
		(* affichage pour le temps pre-niveau *)
		draw_image back.data 0 0;				
		draw_image marcus.imgs.(!marcus_frame).data 160 70;
		print_ropes niveau.ropes point;
		draw_picks niveau.picks spikes;
		print_liens niveau.liens;
		print_ball ball (niveau.points.id 0).x (niveau.points.id 0).y;
		draw_image front.data 0 0;
		synchronize ();
		clear_graph ();
		Unix.select [] [] [] 0.5;
		while !en_jeu do
			if (Sys.time ()) -. !main_time > delta_t then
				begin
				(* time update *)
				main_time := Sys.time ();
				(* calcules *)
				check_rope niveau.points niveau.liens niveau.ropes;
				update_pts niveau.points;
				for i = 0 to bounce do 
					update_liens niveau.liens;
				done;
				if (Sys.time ()) -. !frame_time > 0.10 then
					begin
					marcus_frame := (!marcus_frame + 1) mod marcus.frames;
					slice_frame  := (!slice_frame + 1) mod slice.frames;
					frame_time   := Sys.time ();
					end;
				(* affichage *)
				draw_image back.data 0 0;				
				draw_image marcus.imgs.(!marcus_frame).data 160 70;
				print_ropes niveau.ropes point;
				print_liens niveau.liens;
				draw_picks niveau.picks spikes;
				print_ball ball (niveau.points.id 0).x (niveau.points.id 0).y;
				draw_image front.data 0 0;
				if button_down () then
					begin
					let x_mouse, y_mouse = mouse_pos () in
					draw_image slice.imgs.(!slice_frame).data (x_mouse - 50) (y_mouse - 60);
					end;
				synchronize ();
				clear_graph ();
				end;
			(* checks *)
			if button_down () && Sys.time () -. (!delay) > 0.2 then
				begin
				let indice = contact_lien niveau.liens in
				if indice <> -1 then
					begin
					niveau.liens.remove indice;
					delay := Sys.time ();
					end;
				end;
			if out_screen (niveau.points.id 0).x (niveau.points.id 0).y then
				begin
				is_win := false;
				en_jeu := false;
				end;
			if picks_collision niveau.picks (niveau.points.id 0).x (niveau.points.id 0).y then
				begin
				is_win := false;
				en_jeu := false;
				end;
			if touch_marcus (niveau.points.id 0).x (niveau.points.id 0).y then
				begin
				is_win := true;
				en_jeu := false;
				end;
		done;
		!is_win;;
		
(* fonction de gestion de la page de transition *)
let level_transition marcus bg result niveau =
	let en_cours   = ref true
	and frame      = ref 0
	and frame_time = ref (Sys.time ())
	and menu       = ref false in
	while !en_cours do
		let mouse_x, mouse_y = mouse_pos() in
		(* affichage *)
		draw_image bg.data 0 0;
		set_color white;
		fill_rect 50 200 275 100;
		fill_rect 475 200 275 100;
		set_color black;
		if (mouse_x >= 50 && mouse_x <= 325 && mouse_y >= 200 && mouse_y <= 300) then
			begin
			draw_rect 50 200 275 100;
			end;
		if (mouse_x >= 475 && mouse_x <= 750 && mouse_y >= 200 && mouse_y <= 300) then
			begin
			draw_rect 475 200 275 100;
			end;
		if Sys.win32 then
			begin
			set_text_size 24;
			end;
		if result = true then
			begin
			moveto 375 500;
			draw_string "Win";
			draw_image (marcus.(1)).imgs.(!frame mod (marcus.(1)).frames).data 325 350;
			
			if Sys.file_exists (working_path ^ "\\Niveau\\Niveau-" ^ (string_of_int niveau) ^ ".niv") then
				begin
				moveto 115 240;
				draw_string "Continuer";
				end
			else
				begin 
				moveto 130 240;
				draw_string "Quitter";
				end;
			moveto 585 240;
			draw_string "Menu";
			end
		else
			begin
			moveto 375 500;
			draw_string "Lost !";
			draw_image (marcus.(2)).imgs.(!frame mod (marcus.(2)).frames).data 325 350;
			moveto 135 240;
			draw_string "Rejouer";
			moveto 585 240;
			draw_string "Menu";
			end;
		synchronize ();
		clear_graph ();
		(* updates *)
		if (Sys.time ()) -. !frame_time > 0.10 then
			begin
			frame      := (!frame + 1) mod 20;
			frame_time := Sys.time ();
			end;
		if (mouse_x >= 50 && mouse_x <= 325 && mouse_y >= 200 && mouse_y <= 300) && button_down () then 
			begin
			en_cours := false;
			menu := false;
			end;
		if (mouse_x >= 475 && mouse_x <= 750 && mouse_y >= 200 && mouse_y <= 300) && button_down () then
			begin
			en_cours := false;
			menu := true;
			end;
	done;
	!menu;;
	
	
(* Main *)

(* fonction de lancement *)
let main =
	(* ouverture de la fenêtre *)
	if Sys.win32 then
		begin
		let dimension = (string_of_int largeur) ^ "x" ^ (string_of_int hauteur) in
		open_graph dimension;
		end
	else
		begin
		open_graph "";
		resize_window largeur hauteur;
		end;
	auto_synchronize false;
	display_mode false;
	(* écran de chargement *)
	let slice      = load_brc_set 7 "Images\\Effects\\Slice"
	and ball       = load_brc "Images\\Ball\\Ball"
	and point      = load_brc "Images\\Point\\Point"
	and spike_h    = load_brc "Images\\Spike\\Spike_h"
	and spike_v    = load_brc "Images\\Spike\\Spike_v"
	and back       = load_brc "Images\\Decor\\Back"
	and front      = load_brc "Images\\Decor\\Front" 
	and char1      = load_brc_set 5 "Images\\Dragon\\Waiting"
	and char2      = load_brc_set 5 "Images\\Dragon\\Win"
	and char3      = load_brc_set 4 "Images\\Dragon\\Loose"
	and bgLevel    = load_brc "Images\\Menu\\Backlvl"
	and frameLevel = load_brc "Images\\Menu\\CadreLvl"
	and r_arrow    = load_brc "Images\\Menu\\FlecheDroite" 
	and l_arrow    = load_brc "Images\\Menu\\FlecheGauche" in
	let marcus     = [|char1; char2; char3|]
	and spikes     = [|spike_h; spike_v|]
	(* Constante de jeu *)
	and level      = ref 0 
	and menu       = ref false
	and in_game    = ref true in
		(* boucle du jeu *)
		while !in_game do
			let niveau   = load_level !level in
				let result = partie niveau marcus.(0) ball slice point spikes back front in
				if result = true then
					begin
					incr level;
					end;
					menu := level_transition marcus bgLevel result !level;
			if !menu then
				begin
				level := niveaux bgLevel frameLevel r_arrow l_arrow;
				end
			else if not (Sys.file_exists (working_path ^ "\\Niveau\\Niveau-" ^ (string_of_int !level) ^ ".niv")) then
				begin
				in_game := false;
				close_graph();
				end;
		done;;
		