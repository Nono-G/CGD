open Graph




(** Le type des états internes des sommets **)
type state= BLANC | NOIR | VERT | BLEU | ROUGE | JAUNE | ORANGE | CYAN | VIOLET | FUCHSIA | MARRON | SAPIN | BORDEAUX | MARINE | PRUNE (* blanc ou noir *)

module MyStates=
struct
type t=state (* Les états internes *)
let degree=5 (* On travaille sur des graphes de degré 4 *)
let value=(fun c-> match c with
	| BLANC -> 0
	| NOIR -> 1
	| VERT -> 2
	| BLEU -> 3
	| ROUGE -> 4
	| JAUNE -> 5
	| ORANGE -> 6
	| CYAN -> 7
	| VIOLET -> 8
	| FUCHSIA -> 9
	| MARRON -> 10
	| SAPIN -> 11
	| BORDEAUX -> 12
	| MARINE -> 13
	| PRUNE -> 14) (* une fonction de state -> int pour l'export *)
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
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
