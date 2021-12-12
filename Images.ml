#load "graphics.cma";;
open Graphics;;
Graphics.open_graph "800x600";;

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
  		
let raw_image = lecture_png "C:\\Users\\thoma\\OneDrive\\Bureau\\Informatique\\Projet_Caml\\DM -cut the rope\\ball.txt" 50 50;;

let image = make_image raw_image;;

clear_graph ();
draw_image image 50 50;;