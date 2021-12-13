#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;

let working_path = "C:\\Users\\thoma\\OneDrive\\Bureau\\Informatique\\Projet_Caml\\DM - cut the rope\\";;

type img = {data: image; height: int; width: int};;
type gif = {imgs: img array; frames: int};;

(* Fonctions d'importation *)
(* Pour ces imports il faut absolument ouvrir une fenêtre *)

let load_brc relative_link =
	let file  = open_in (working_path ^ relative_link ^ ".brc") in
	let height, width = Scanf.sscanf (input_line file) "%dx%d" (fun a b -> (a, b)) in
	let pre_image = Array.make_matrix height width transp in
		for i = 0 to height - 1 do
			for j = 0 to width - 1 do
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
  		done;
  		{data = make_image pre_image; height = height; width = width};;

let load_brc_set frames relative_link =
	let empty_image = {data = make_image [|[|-1|]|]; height = 0; width = 0} in
	let gif_image   = {imgs   = Array.make frames empty_image;
						    frames = frames} in
	for i = 1 to frames do
		let path      = relative_link ^ ("-" ^ (string_of_int i)) in
		gif_image.imgs.(i-1) <- load_brc path;
	done;
	gif_image;;


let main =
	open_graph "800x600";
	auto_synchronize false;
	display_mode false;
	set_color black;
	fill_rect 0 0 800 600;
	moveto 500 30;
	set_color white;
	set_text_size 40;
	draw_string "Loading...";
	synchronize ();
	let gif1 = load_brc_set 5 "Images\\Dragon\\Win"
	and gif2 = load_brc_set 5 "Images\\Dragon\\Waiting"
	and gif3 = load_brc_set 4 "Images\\Dragon\\Loose"
	and ball = load_brc "Images\\Ball\\Ball"
	and id   = ref 0
	and time = ref (Unix.gettimeofday ()) in
		fill_rect 0 0 800 600;
		synchronize ();
		while true do
			draw_image gif1.imgs.(!id mod gif1.frames).data 50 50;
			draw_image gif2.imgs.(!id mod gif2.frames).data 200 50;
			draw_image gif3.imgs.(!id mod gif3.frames).data 350 50;
			draw_image ball.data 50 200;
			if (Unix.gettimeofday ()) -. !time > 0.10 then
				begin
				id  := (!id + 1) mod 20;
				time := Unix.gettimeofday ();
				end;
			synchronize ();
			clear_graph ();
		done;;