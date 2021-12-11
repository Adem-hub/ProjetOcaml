#load "graphics.cma";;
open Graphics;;
open Array;;

open_graph "";
set_color (rgb 255 0 255);;
let cosh x = let expo = exp(x) in (expo+.(1./.expo))/.2.;;
let sinh x = let expo = exp(x) in (expo-.(1./.expo))/.2.;;
let arctanh x = (1./.2.)*.log((1.+.x)/.(1.-.x));;
let d_xy x1 x2 = (x2-.x1);;
let moy_xy x1 x2 = (x2+.x1)/.2.;;

let d= ref 0.001;;
let dA=0.001;;
let l=ref 300.;;

let x1=100.;;
let x2=200.;;
let y1=200.;;
let y2=400.;;

let dx = d_xy x1 x2;;
let dy = d_xy y1 y2;;

let mx= moy_xy x1 x2;;

type coordonnees= {x:int;y:int;oldx:int;oldy:int};;
type 'a listeCoords = {mutable taille:int; mutable tab:  'a array};;


let ajoute liste valeur = let prov= make (liste.taille*2) valeur and taille=liste.taille in
								  if length (liste.tab) > liste.taille then begin
								  liste.tab.(taille)<-valeur;
								  liste.taille<-liste.taille+1;
								  end
								  else begin 
								  for i=0 to (liste.taille)-1 do
										prov.(i)<- liste.tab.(i);
										done;
									liste.taille<-liste.taille+1;
									prov.(taille)<-valeur;
									liste.tab<-prov;
									end;;

let gA=	
			let r=sqrt(!l**2.-.dy**2.)/.dx in
			let left=ref (r*.(!d))
			and right=ref(sinh(!d)) in
			while ((!left)>=(!right)) do
				left:= r*.(!d);
				right:= sinh(!d);
				d:=!d+.dA
				done; d:=!d-.dA;!d;;

let a=dx/.(2.*.gA);;
let b=mx-.(a*.arctanh(dy/.(!l)));;
let c=y1-.a*.cosh((x1-.b)/.a);;

let x=ref x1;;
let y= ref y1;;
let ddx=6.;;

let tableauCoords = {taille=2;tab=[| {x = (int_of_float(x1)) ;y = int_of_float(y1); oldx = int_of_float(x1 -.1.); oldy = int_of_float(y1 -.1.)};{x = int_of_float(x2); y = int_of_float(y2); oldx = int_of_float(x2 -.1.); oldy = int_of_float(y2 -.1.)}|]} in
if ((!l)**2.)>=(dx**2.+.dy**2.) then
	while (!x<=x2) do
	  moveto (int_of_float(!x)) (int_of_float(!y));
	  y:= a*.cosh((!x-.b)/.a)+.c;
	  x:= !x+.ddx;
	  lineto (int_of_float(!x)) (int_of_float(!y));
	  ajoute tableauCoords {x=(int_of_float !x) ; y = (int_of_float !y) ; oldx=int_of_float (!x -.1.) ; oldy = int_of_float (!y -.1.)};
	  done
else trace (int_of_float(x1)) (int_of_float(y1)) (int_of_float(x2)) (int_of_float(y2));;
