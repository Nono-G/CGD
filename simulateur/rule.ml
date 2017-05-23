open Graph

type state= SAPIN|VERT|MENTHE|ANIS|
			MARINE|BLEU|CIEL|CYAN|
			BORDEAUX|ROUGE|TOMATE|ROSE|
			PRUNE|VIOLET|LAVANDE|FUCHSIA|
			NOIR|GRIS|ACIER|BLANC|
			MARRON|ORANGE|FEU|JAUNE|
			KAKI

module MyStates=
struct
type t=state (* Les états internes *)
let degree=5 (* On travaille sur des graphes de degré 4 *)
let value=(fun c-> match c with
	| SAPIN -> 0
	| VERT -> 1
	| MENTHE -> 2
	| ANIS -> 3

	| MARINE -> 4
	| BLEU -> 5
	| CIEL -> 6 
	| CYAN -> 7
	
	| BORDEAUX -> 8
	| ROUGE -> 9
	| TOMATE -> 10
	| ROSE -> 11
	
	| PRUNE -> 12
	| VIOLET -> 13
	| LAVANDE -> 14
	| FUCHSIA -> 15
	
	| NOIR -> 16
	| GRIS -> 17
	| ACIER -> 18
	| BLANC -> 19
	
	| MARRON -> 20
	| ORANGE -> 21
	| FEU -> 22
	| JAUNE -> 23
	
	| KAKI -> 24
	)
(* une fonction de state -> int pour l'export *)
end


(** J'utilise Graph.Make pour créer mon type de graphes dont les sommets ont MyStates comme états internes **)
module MyGraphs = Graph.Make(MyStates)


(** On définit la CGD comme le mapping de la rule sur tous les sommets du graphe **)


let run_rule rule g= (* Cette fonction mange une règle + un graphe et renvoie le graphe image *)
let ng= (MyGraphs.create ()) in 
let (_, vtbl,_)= g in
let tmp k v _ =MyGraphs.union ng (rule  v g) in
Hashtbl.fold tmp vtbl ();ng



let copy g g2=
let (_,h,hs)=g and  (_,h2,hs2)=g2 in
Hashtbl.clear h;
Hashtbl.clear hs;
let tmp k v _=Hashtbl.add h k v in
let tmps k v _=Hashtbl.add hs k v in
Hashtbl.fold tmp h2 ();
Hashtbl.fold tmps hs2 ()

(*
let nG=MyGraphs.create ()

let run_rule_en_place rule = (* Celle ci modifie le nG en place. Elle ne renvoie rien *)
	let ng= (MyGraphs.create ()) in 
		let (_, vtbl,_)= ng in
		let tmp k v _ =MyGraphs.union ng (rule  v nG) in
			Hashtbl.fold tmp vtbl (); copy nG ng
		
		*)	
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
