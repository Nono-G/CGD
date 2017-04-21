open Graph
open Rule

(*(** Le type des états internes des sommets **)
type state= BLANC | NOIR (* blanc ou noir *)

module MyStates=
struct
	type t=state (* Les états internes *)
	let degree=4 (* On travaille sur des graphes de degré 4 *)
	let value=(fun c-> match c with BLANC -> 0 | NOIR -> 1) (* une fonction de state -> int pour l'export *)
end*)


(** J'utilise Graph.Make pour créer mon type de graphes dont les sommets ont MyStates comme états internes **)
module MyGraphs = Graph.Make(Rule.MyStates)

(*Situation initiale pour Moving Head*)
let init_graph n =
	let ng=MyGraphs.create () in
	for i=1 to n do
		let nom = ("c"^(string_of_int i)) in
		MyGraphs.addVertex ng (MyGraphs.createVertex nom);
		MyGraphs.setState ng nom Rule.(NOIR);
	done;
	for i=1 to (n-1) do
		let nom1 = ("c"^(string_of_int i)) in
		let nom2 = ("c"^(string_of_int (i+1))) in
		MyGraphs.addEdge ng (nom1,0) (nom2,1);
	done;
	MyGraphs.addVertex ng (MyGraphs.createVertex "h");
	MyGraphs.addEdge ng ("h",2) ("c1",2);
	MyGraphs.setState ng "h" Rule.(BLANC);
	(*retour -> *)ng;;

(*Moving Head : GCD un sommet 'tête' se déplace le long d'une (0;1)-chaîne 'ruban'*)
let cgd_rule v g =
	let ng = MyGraphs.create() in (*Création d'un nouveau graphe*)
	let nom = fst v in
	MyGraphs.addVertex ng (MyGraphs.createVertex nom);(*On va conserver les mêmes sommets*)
	(match MyGraphs.getState g nom with
		|BLANC->(*C'est la tête*)
		MyGraphs.setState ng nom Rule.(BLANC);(*La tête est dans l'état BLANC*)
		(match MyGraphs.getNeighbour v 2 with (*La tête est-elle reliée par son port 2 (~c) ?*)
			|None -> ()
			|Some(u,2)->	(*relié à un sommet u par un liaison 2-2 (~cc)*)
			(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 0 with(* u est-il relié à un voisin par son port 0 (~a) ?*)
				(* i.e. : Est-ce la fin de la chaîne ?*)
				|None -> (* fin de la chaîne *)
					(*On bascule 'tête en bas' avec une liaison 3-3 (~dd)*)
					MyGraphs.addVertex ng (MyGraphs.createVertex u); (*On anticipe la création de u pour pouvoir créer l'arrête*)
					MyGraphs.addEdge ng (nom, 3) (u,3) (*On crée l'arrête 3-3 (~dd*)
				|Some(w,1) -> (*u a un successeur w dans la chaine *)
					(*On se connecte a w par une liaison 2-2 (~cc)*)
					MyGraphs.addVertex ng (MyGraphs.createVertex w); (*On anticipe la création de w pour pouvoir créer l'arrête*)
					MyGraphs.addEdge ng (nom,2) (w,2) (*On crée l'arrête*)
				|_->()(*Cas normalement inateignable car la chaine est constituée de liaisons 0-1 (~ab) uniquement*)
			);
			|_->()(*Cas normalement inateignable car si la tête utilise son port 2 (~c) alors le sommet de la chaine relié aussi*)
		);
		(match MyGraphs.getNeighbour v 3 with (*La tête est-elle reliée par son port 3 (~d) ?*)
			|None -> ()
			|Some(u,3)-> (*relié à un sommet u par un liaison 3-3 (~dd)*)
			(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 1 with (* u est-il relié à un voisin par son port 1 (~b) ?*)
				(* i.e. : sommes nous revenus au début de la chaîne ?*)
				|None -> (* début de la chaîne *)
					(*On bascule 'tête en haut' avec une liaison 2-2 (~cc)*)
					MyGraphs.addVertex ng (MyGraphs.createVertex u); (*On anticipe la création de u pour pouvoir créer l'arrête*)
					MyGraphs.addEdge ng (nom, 2) (u,2) (*On crée l'arrête 2-2 (~cc)*)
				|Some(w,0) -> (*u a un prédécesseur w dans la chaine *)
					(*On se connecte a w par une liaison 3-3 (~dd)*)
					MyGraphs.addVertex ng (MyGraphs.createVertex w); (*On anticipe la création de w pour pouvoir créer l'arrête*)
					MyGraphs.addEdge ng (nom, 3) (w,3) (*On crée l'arrête 3-3 (~dd)*)
				|_->()(*Cas normalement inateignable car la chaine est constituée de liaisons 0-1 (~ab) uniquement*)
			);
			|_->()(*Cas normalement inateignable car si la tête utilise son port 3 (~d) alors le sommet de la chaine relié aussi*)
		);
		|NOIR->(*Ce n'est pas la tête*)
		MyGraphs.setState ng nom Rule.(NOIR);(*Les sommets de la chaine sont dans l'état NOIR*)
		(match MyGraphs.getNeighbour v 0 with (* ce sommet est-il relié par son port 0 (~a) ?*)
			|None -> () (*Fin de la chaîne*)
			|Some(u,1)-> (*v est connecté au port 1 (~b) d'un sommet u *)
				MyGraphs.addVertex ng (MyGraphs.createVertex u); (*On anticipe la création de u pour pouvoir créer l'arrête*)
				MyGraphs.addEdge ng (nom,0) (u,1) (*On crée l'arrête 0-1 (~ab) entre v et u*)
			|_->()(*Cas normalement inateignable car la chaine est constituée de liaisons 0-1 (~ab) uniquement*)
		);
		|_->()(*On utilise que blanc et noir dans cet exemple*)
	);
	(**retour->*)ng;;