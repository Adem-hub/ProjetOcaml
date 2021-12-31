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
let marcus_x     = 160;;
let marcus_y     = 70;;

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
                             remove : int  -> unit;
                             switch : int  -> int -> unit};;

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
	and change indice valeur =
		(!support).(indice) <- valeur;
	in {size   = (fun () -> !taille);
	    id     = (fun i -> if i < !taille then !support.(i) else failwith "Index out of range");
	    add    = ajoute;
	    remove = supprime;
	    switch = change};;


(* Fonctions outils *)

let distance vct1 vct2 = sqrt((vct1.x -. vct2.x)**2. +. (vct1.y -. vct2.y)**2.);;

(* renvoie l'id de bout touché et -1 si aucun n'est touché*)
let rope_touch rope x y =
	let id = ref (-1) in
	for i = 0 to rope.size() - 1 do
		let xi = (rope.id (i)).debut.x
		and yi = (rope.id (i)).debut.y
		and xf = (rope.id (i)).fin.x
		and yf = (rope.id (i)).fin.y in
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
	let xi = (float_of_int marcus_x) -. 90.
	and yi = (float_of_int marcus_y) -. 90.
	and xf = (float_of_int marcus_x) +. 90.
	and yf = (float_of_int marcus_y) +. 90. in
	if (xi -. x) *. (xf -. x) <= 0. && (yi -. y) *. (yf -. y) <= 0. then
		true
	else
		false;;


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
	let empty_image = {data = make_image [|[|-1|]|];
	                   height = 0;
			   width = 0} in
	let gif_image   = {imgs   = Array.make frames empty_image;
	                   frames = frames} in
	for i = 1 to frames do
		let path = relative_link ^ ("-" ^ (string_of_int i)) in
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
	and ball   = load_brc "Images\\Ball\\Test"
	and back   = load_brc "Images\\Decor\\Back"
	and front  = load_brc "Images\\Decor\\Front" in
	let char   = [|char1; char2; char3|]
	and x_ball = ref (Random.int 740); (*370*)
	and y_ball = ref 800
	and mode   = ref 1
	and id     = ref 0
	and time1  = ref (Unix.gettimeofday ())
	and time2  = ref (Unix.gettimeofday ())
	and en_jeu = ref true in
		(* boucle du jeu *)
		while true do
			if (Unix.gettimeofday ()) -. !time1 > 0.01 then
				begin
				(* préparation affichage *)
				draw_image back.data 0 0;
				let frame = !id mod (char.(!mode)).frames 
				and x     = if !mode = 2 then marcus_x - 15 else marcus_x
				and y     = if !mode = 2 then marcus_y - 20 else marcus_y in
				draw_image (char.(!mode)).imgs.(frame).data x y;
				if button_down () then
						begin
						let frame = !id mod slice.frames
						and x_mouse, y_mouse = mouse_pos () in
						draw_image slice.imgs.(frame).data (x_mouse - 50) (y_mouse - 50);
						end;
				
				draw_image ball.data (!x_ball) (!y_ball);
				draw_image front.data 0 0;
				if !en_jeu = false then
					begin
					if !mode = 0 then
						begin
						set_color black;
						fill_rect 349 490 102 50;
						set_color white;
						set_text_size 30;
						moveto 357 500;
						draw_string "Win !";
						end
					else
						begin
						set_color black;
						fill_rect 330 490 136 50;
						set_color white;
						set_text_size 30;
						moveto 337 500;
						draw_string "Loose !";
						end;
					end;
				(* actualisation varibales *)
				if (Unix.gettimeofday ()) -. !time1 > 0.10 then
					begin
					id    := (!id + 1) mod 140; (* PPCM de tous les animations *)
					time1 := Unix.gettimeofday ();
					end;
				if !en_jeu = true then
					begin
					if (!mode) <> 0 then
						begin
						x_ball := !x_ball - 2;
						y_ball := !y_ball - 5;
						end;
					if out_screen (float_of_int(!x_ball)) (float_of_int(!y_ball)) then
						begin
						mode   := 2;
						en_jeu := false;
						end;
					if touch_marcus (float_of_int(!x_ball)) (float_of_int(!y_ball)) then
						begin
						mode   := 0;
						x_ball := -100;
						y_ball := -100;
						en_jeu := false;
						end;
					end
				else
					begin
					if button_down () then
						begin
						mode   := 1;
						en_jeu := true;
						x_ball := Random.int 740;
						y_ball := 800;
						end;
					end;
				(* affichage *)
				synchronize ();
				clear_graph ();
				time2 := Unix.gettimeofday ();
				end;
		done;;
