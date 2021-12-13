#load "graphics.cma";;
open Graphics;;

let working_path = "C:\\Users\\thoma\\OneDrive\\Bureau\\Informatique\\Projet_Caml\\DM - cut the rope\\";;

type music = {data: (int*int) array; taille: int};;

(* Fonctions d'importation *)

let load_at relative_link =
	let file   = open_in (working_path ^ relative_link ^ ".at") in
	let notes  = Scanf.sscanf (input_line file) "%d" (fun a -> a) in
	let audio  = Array.make notes (0, 0) in
		for i = 0 to notes - 1 do
			let line = input_line file in
			let note, rep = Scanf.sscanf line "%s %d" (fun a b -> (a, b)) in
			if note = "do" then
				audio.(i) <- (261, rep)
			else if note = "re" then
				audio.(i) <- (293, rep)
			else if note = "mi" then
				audio.(i) <- (329, rep)
			else if note = "fa" then
				audio.(i) <- (349, rep)
			else if note = "sol" then
				audio.(i) <- (392, rep)
			else if note = "la" then
				audio.(i) <- (440, rep)
			else if note = "si" then
				audio.(i) <- (493, rep);
  		done;
  		{data = audio; taille = notes};;

let play song =
	for i = 0 to song.taille - 1 do
		let note, rep = song.data.(i) in
		sound note (rep*150);
	done;;

let song = load_at "Music\\test";;

play song;;