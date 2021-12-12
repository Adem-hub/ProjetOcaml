#load "graphics.cma";;
#load "unix.cma";;
open Graphics;;
open_graph "800x600";;

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

(* Pour ces imports il faut absolument ouvrir une fenêtre *)

let frames1 = Array.make 5 (make_image ([|[|-1|]|]));;

for i = 1 to 5 do
	let start     = "C:\\Users\\thoma\\OneDrive\\Bureau\\Informatique\\Projet_Caml\\DM - cut the rope\\Images\\Dragon - animation\\Win-"^(string_of_int i) in
	let access    = start^".txt" in
	let raw = lecture_png access 150 150 in
	frames1.(i-1) <- (make_image raw);
done;;

let frames2 = Array.make 5 (make_image ([|[|-1|]|]));;

for i = 1 to 5 do
	let start     = "C:\\Users\\thoma\\OneDrive\\Bureau\\Informatique\\Projet_Caml\\DM - cut the rope\\Images\\Dragon - animation\\Waiting-"^(string_of_int i) in
	let access    = start^".txt" in
	let raw = lecture_png access 150 150 in
	frames2.(i-1) <- (make_image raw);
done;;

let frames3 = Array.make 5 (make_image ([|[|-1|]|]));;

for i = 1 to 4 do
	let start     = "C:\\Users\\thoma\\OneDrive\\Bureau\\Informatique\\Projet_Caml\\DM - cut the rope\\Images\\Dragon - animation\\Loose-"^(string_of_int i) in
	let access    = start^".txt" in
	let raw = lecture_png access 150 150 in
	frames3.(i-1) <- (make_image raw);
done;;

auto_synchronize false;
display_mode false;
let id1  = ref 0
and id2  = ref 0 
and time = ref (Unix.gettimeofday ()) in
	while true do
		draw_image frames1.(!id1) 50 50;
		draw_image frames2.(!id1) 200 50;
		draw_image frames3.(!id2) 350 50;
		if (Unix.gettimeofday ()) -. !time > 0.10 then
			begin
			id1  := (!id1 + 1) mod 5;
			id2  := (!id2 + 1) mod 4;
			time := Unix.gettimeofday ();
			end;
		synchronize ();
		clear_graph ();
	done;;