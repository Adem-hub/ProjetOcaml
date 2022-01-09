#load "graphics.cma";;

#load "unix.cma";;

open Graphics;;
open Printf;;

open_graph ("800x1000");;
auto_synchronize false;;
display_mode false;;



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
		taille := !taille - 1;
	in {size     = (fun () -> !taille);
		 id       = (fun i -> if i < !taille then !support.(i) else failwith "Index out of range");
		 add      = ajoute;
		 remove   = supprime};;



let display_menu true = 
	set_color blue;
	fill_rect 260 300 275 100;
	fill_rect 260 500 275 100;;

let display_buttons =
	set_color blue;
	fill_rect 190 30 60 30;
	fill_rect 530 30 60 30;;
	
let set_levels =
	let level  = ref 0
	and x      = ref 45
	and y      = ref 800 
	and tab    = ref (make_tab (0,0)) in
	let levels = make_tab (!tab) in
		while !level < 66 (*ici la condition quand on parcourt les fichiers *) do
			(!tab).add (!x, !y);
			x := !x+ 90;
			if !x >= 720 then begin
				x := 45;
				y := !y - 100
				end;
			if !y <= 100 then begin
				y := 800;
				levels.add (!tab);
				tab := make_tab (0,0);
				end;
			incr level;
			done;
			levels.add (!tab);
			levels;;



let display_levels levels page =
	set_color red;
	let tab = (levels.id page) in
	for i = 0 to tab.size() - 1 do
		let x, y = (tab.id i) in
			fill_rect (x) (y) 75 75;
			set_text_size 40;
			set_color white;
			moveto (x+10) (y+10);
			draw_string (string_of_int(i + page*56));
			set_color red;
		done;
		set_color blue;
		fill_rect 190 30 74 30;
		fill_rect 530 30 74 30;;

let detect_level levels page pressed= 
	let tab = (levels.id page) in
		for i = 0 to tab.size() - 1 do
			let x, y   = (tab.id i)
			and px, py = mouse_pos() in
				if (px >= x && px <= x + 75 && py >= y && py <= y + 75) && button_down () then
					begin
					(* ici on met le niveau *)
					print_int (i + 30*page);
					print_newline();
					pressed := true;
					end;
	done;;

let change_page page bool max= 
	let px, py = mouse_pos() 
	and coords = [|210;510|]  in 
		for i = 0 to 1 do 
			if (px >= coords.(i) && px <= coords.(i) + 30 && py >= 30 && py <= 50) then
					begin
					if button_down () && not (!bool) then 
					   begin
						bool := true;
						if i = 0 && !page > 0 then 
							decr page;
							clear_graph ();
						if i = 1 && !page < max then
							incr page;
							clear_graph ();
						display_buttons;
						end;
						
					if not(button_down ()) && (!bool) then
						bool := false;
					end;
		done;;





let niveaux = 
	let levels_tab    = set_levels 
	and page          = ref 0 
	and level_pressed = ref false 
	and bool = ref false in
		while not (!level_pressed) do
			display_levels levels_tab (!page);
			detect_level levels_tab (!page) level_pressed;
			change_page page bool (levels_tab.size() -1);
			display_buttons;
			synchronize ();
	done;;





