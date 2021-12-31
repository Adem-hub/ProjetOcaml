(* Import *)

#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open Array;;


(* Constantes *)

let working_path = "C:\\Users\\admin\\Desktop\\WinCaml\\";;
let cst_rebond   = 3;;
let g            = 9.81;;
let hauteur      = 1000;;
let largeur      = 800;;

(* Types *)

type img  = {data   : image;
			    height : int;
				 width  : int};;
					
type gif  = {imgs   : img array;
				 frames : int};;

type vct  = {mutable x    : float;
			    mutable y    : float;
				 mutable oldx : float;
				 mutable oldy : float};;

type lien = {mutable debut  : vct;
			    mutable fin    : vct;
				 mutable taille : float};;
					
type 'a tableau_dynamique = {size   : unit -> int;
									  id     : int  -> 'a;
									  add    : 'a   -> unit;
									  remove : int  -> unit};;

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
	in {size   = (fun () -> !taille);
		 id     = (fun i -> if i < !taille then !support.(i) else failwith "Index out of range");
		 add    =  ajoute;
		 remove =  supprime};;


(* Fonctions outils *)

let distance vct1 vct2 = sqrt((vct1.x -. vct2.x)**2. +. (vct1.y -. vct2.y)**2.);;

(* renvoie l'id de bout touché et -1 si aucun n'est touché*)
let rope_touch rope x y =
	let id = ref (-1) in
	for i = 0 to rope.size() - 1 do
		let x1 = (rope.id (i)).debut.x
		and y1 = (rope.id (i)).debut.y
		and x2 = (rope.id (i)).fin.x
		and y2 = (rope.id (i)).fin.y in
		if (x1 -. x) *. (x2 -. x) <= 0. && (y1 -. y) *. (y2 -. y) <= 0. then
			id := i;
	done;
	!id;;

let out_screen x y =
	let test = ref false in
	if y > hauteur + 100 then
		test := true;
	if y < -100 then
		test := true;
	if x > largeur + 100 then
		test := true;
	if x < -100 then
		test := true;
	!test;;
	

(* Fonctions grphiques *)

let chargement pourcentage fichier = 
	let couleur = rgb 25 85 110 in
	set_color couleur;
	fill_rect 0 0 largeur hauteur;
	set_color white;
	set_text_size 12;
	moveto 10 30;
	draw_string fichier;
	set_text_size 40;
	moveto 500 30;
	draw_string "Loading...";
	set_text_size 14;
	moveto 100 800;
	draw_string "This story is about Marcus, a small and hungry dinosaur. Marcus was";
	moveto 100 750;
	draw_string "walking in the forest and lost himself. He isn't so worried about";
	moveto 100 700;
	draw_string "finding his way home. Perhaps, as a dinosaur, Marcus got a fine sense";
	moveto 100 650;
	draw_string "of smell. But something still bothers him; he hasn't eaten in a while.";
	moveto 100 600;
	draw_string "He starts looking around and arrives near a montain. There, Marcus was";
	moveto 100 550;
	draw_string "surprised to see a wood escalator. Were escalators growing in trees,";
	moveto 100 500;
	draw_string "perhaps not it was a sign of life. Marcus uses it to climb the montane";
	moveto 100 450;
	draw_string "and see the beautiful old house where you live. He knocks on the door.";
	moveto 100 400;
	draw_string "You open and see this desperate dinosaur. You grab your magic wand, which";
	moveto 100 350;
	draw_string "can cut any rope at any distance, and help him get a pleasant dinner.";
	let charge = int_of_float (float_of_int largeur *. pourcentage) in
	fill_rect 0 0 charge 20;
	synchronize ();;


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
	let gif1  = load_brc_set 5 "Images\\Dragon\\Win"
	and gif2  = load_brc_set 5 "Images\\Dragon\\Waiting"
	and gif3  = load_brc_set 4 "Images\\Dragon\\Loose"
	and ball  = load_brc "Images\\Ball\\Test"
	and back  = load_brc "Images\\Decor\\Back"
	and front = load_brc "Images\\Decor\\Front" in
	let gifs  = [|gif1; gif2; gif3|]
	and mode  = ref 0
	and id    = ref 0
	and time1 = ref (Unix.gettimeofday ())
	and time2 = ref (Unix.gettimeofday ()) in
		set_color red;
		(* boucle du jeu *)
		while true do
			draw_image back.data 0 0;
			let frame = !id mod (gifs.(!mode)).frames 
			and x     = if !mode = 2 then 145 else 160
			and y     = if !mode = 2 then 50 else 70 in
				draw_image (gifs.(!mode)).imgs.(frame).data x y;
			draw_image ball.data 370 800;
			draw_image front.data 0 0;
			let x_mouse, y_mouse = mouse_pos () in
				fill_circle x_mouse y_mouse 5;
			if (Unix.gettimeofday ()) -. !time1 > 0.10 then
				begin
				id    := (!id + 1) mod 20;
				time1 := Unix.gettimeofday ();
				end;
			if (Unix.gettimeofday ()) -. !time2 > 5. then
				begin
				mode  := (!mode + 1) mod 3;
				time2 := Unix.gettimeofday ();
				end;
			synchronize ();
			clear_graph ();
			resize_window largeur hauteur;
		done;;
