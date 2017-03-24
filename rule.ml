open Graph




(** Le type des états internes des sommets **)
type state= BLANC | NOIR (* blanc ou noir *)

module MyStates=
	struct
		type t=state (* Les états internes *)
		let degree=4 (* On travaille sur des graphes de degré 4 *)
		let value=(fun c-> match c with BLANC -> 0 | NOIR -> 1) (* une fonction de state -> int pour l'export *)
	end


(** J'utilise Graph.Make pour créer mon type de graphes dont les sommets ont MyStates comme états internes **)
module MyGraphs = Graph.Make(MyStates)



(** L'exemple suivant implémente une CGD sur des graphes de degré 4 qui transforme chaque sommet 'toto' en 4 sommets 'toto.1','toto.2','toto.3' et 'toto.4' **)
(** Elle recolle ensuite les patchs de 4 sommets ensemble en respectant la structure initiale du graphe **)


(** la règle mange 1 sommet et 1 graphe et renvoie un graphe **)
(** pour simplifier la règle reçoit tout le graphe, c'est donc à l'utilisateur de n'utiliser que le voisinage de v pour garantir la localité de la règle **)
let my_rule v g=
	let ng=MyGraphs.create () in (* ng est le graphe vide que je vais remplir au fur et à mesure *)
		MyGraphs.addVertex ng (MyGraphs.createVertex ((fst v)^".1")); (* Je rajoute mes 4 sommets issus de v. (fst v) est le nom de v (c'est une string) *)
		MyGraphs.addVertex ng (MyGraphs.createVertex ((fst v)^".2")); 
		MyGraphs.addVertex ng (MyGraphs.createVertex ((fst v)^".3")); 
		MyGraphs.addVertex ng (MyGraphs.createVertex ((fst v)^".4")); 
		MyGraphs.addEdge ng (((fst v)^".1") , 1) (((fst v)^".2"), 3); (* Je rajoute les 4 arrêtes pour former un carré avec ces 4 sommets *)
		MyGraphs.addEdge ng (((fst v)^".2") , 2) (((fst v)^".3"), 0); (* 'toto.2' est relié à 'toto.3' par le port 2 et arrive sur le port 0 *)
		MyGraphs.addEdge ng (((fst v)^".3") , 3) (((fst v)^".4"), 1);
		MyGraphs.addEdge ng (((fst v)^".4") , 0) (((fst v)^".1"), 2);
		(** Maintenant ça devient plus compliqué: on veut tester si v avait un voisin sur le port p (pour p dans {0,1,2,3}) **)
		(** Si oui, alors il faut lui dire comment se rattacher aux sommets issus de ce voisin**)
		for p = 0 to 3 do
				match MyGraphs.getNeighbour v p with
				None ->() (* si pas de voisin, on fait rien *)
				|Some (u,pp)-> (* si il y a un voisin nommé u qui me voit par le port pp*)
					(* le matching suivant est là pour savoir quels sont les 2 sommets (parmis les 4) que notre sommet v va voir du côté de u *)
					(* En gros la convention est la suivante: 
						- si p=0 alors toto.1 et toto.2 verront les 2 sommets en questions
						- si p=1 alors toto.2 et toto.3 __________________________________
						- si p=2 alors toto.3 et toto.4 __________________________________
						- si p=3 alors toto.4 et toto.1 __________________________________
					ET
						- si pp=0 alors les deux fils de v vont voir u.2 et u.1 (dans cet ordre)
						- si pp=1 alors les deux fils de v vont voir u.3 et u.2 (dans cet ordre)
						- si pp=2 alors les deux fils de v vont voir u.4 et u.3 (dans cet ordre)
						- si pp=3 alors les deux fils de v vont voir u.1 et u.4 (dans cet ordre) *)
					(* Ça se voit mieux sur un dessins ;D *)
				let nameu1,nameu2=(
					match pp with
					|0 -> (".2",".1")
					|1 ->(".3",".2")
					|2 ->(".4",".3")
					|_ ->	(".1",".4"))
					and namev1,namev2=(
					match p with
					|0 -> (".2",".1")
					|1 ->(".3",".2")
					|2 ->(".4",".3")
					|_ ->	(".1",".4"))
					 in (*Je génère les noms en fonctions de pp*)
					MyGraphs.addVertex ng (MyGraphs.createVertex (u^nameu1));(*Je génère les sommets*)
					MyGraphs.addVertex ng (MyGraphs.createVertex (u^nameu2));
					MyGraphs.addEdge ng (((fst v)^namev2) , p) (u^nameu1, pp);(*Je les connecte aux bons sommets initiaux*)
					MyGraphs.addEdge ng (((fst v)^namev1) , p) (u^nameu2, pp);
					
		done;
		(** Je rajoute de la couleur à tout ça **)
		MyGraphs.setState ng ((fst v)^".2") (BLANC); (*Le sommet en haut à droite est toujours blanc *)
		(* les autres reçoivent la couleur de v *)
		let color=MyGraphs.getState g (fst v) in
		MyGraphs.setState ng ((fst v)^".1") (color);
		MyGraphs.setState ng ((fst v)^".3") (color);
		MyGraphs.setState ng ((fst v)^".4") (color);
		(* La règle n'emmet pas d'avis sur les couleurs des voisins ! *)
		ng (*pour finir on renvoie le graphe qu'on vient de générer *)
		
		
		
		
		
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
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
