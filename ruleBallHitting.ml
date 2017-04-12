open Graph
open Rule


(** J'utilise Graph.Make pour créer mon type de graphes dont les sommets ont MyStates comme états internes **)
module MyGraphs = Graph.Make(Rule.MyStates)

(*Situation initiale pour BALL HITTING*)
let init_graph () =
	let ng=MyGraphs.create () in
	for i=0 to 4 do
		let nom = ("g"^(string_of_int i)) in
		MyGraphs.addVertex ng (MyGraphs.createVertex nom);
		MyGraphs.setState ng nom Rule.VERT;
	done;
	for i = 0 to 3 do
		let n1 = ("g"^(string_of_int i)) in
		let n2 = ("g"^(string_of_int (i+1))) in
		MyGraphs.addEdge ng (n1,0) (n2,1);
	done;
	MyGraphs.addVertex ng (MyGraphs.createVertex "b");
	MyGraphs.setState ng "b" Rule.BLANC;
	MyGraphs.addEdge ng ("b", 3) ("g3", 3);
	MyGraphs.addVertex ng (MyGraphs.createVertex "h1");
	MyGraphs.setState ng "h1" Rule.NOIR;
	MyGraphs.addVertex ng (MyGraphs.createVertex "h2");
	MyGraphs.setState ng "h2" Rule.JAUNE;
	MyGraphs.addEdge ng ("h1", 3) ("h2", 3);
	MyGraphs.addEdge ng ("h2", 2) ("g3", 2);
	(*retour -> *)ng;;

let cgd_rule v g =
	let ng = MyGraphs.create () in
	let idv = fst v in
	(*print_string (idv^" \n");flush_all ();*)
	MyGraphs.addVertex ng (MyGraphs.createVertex idv);
	(match MyGraphs.getState g idv with
		|Rule.VERT->(*PELOUSE*)
			MyGraphs.setState ng idv Rule.VERT;
			(match MyGraphs.getNeighbour v 0 with
				|None->()(*Pas de sucesseur avant, pas non plus après*)
				|Some(u,p)->(*un successeur*)
					MyGraphs.addVertex ng (MyGraphs.createVertex u);
					MyGraphs.addEdge ng (idv, 0) (u, 1)
				);
		|Rule.BLANC->(*BALLE IMMOBILE*)
			(match MyGraphs.getNeighbour v 3 with
				|None -> () (*Pas normal*)
				|Some(u,p)->
					MyGraphs.addVertex ng (MyGraphs.createVertex u);
					MyGraphs.addEdge ng (idv,3) (u,3);
					(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 2 with
						|None->MyGraphs.setState ng idv Rule.BLANC
						|Some(w,pp)->
							(match MyGraphs.getState g w with
								|ROUGE->MyGraphs.setState ng idv Rule.BLEU (*La balle se met en mouvement*)
								|_->MyGraphs.setState ng idv Rule.BLANC (*La balle reste sur place*)
							);
					);
			);
		|BLEU->(*BALLE EN MOUVEMENT ALLER*)
			(match MyGraphs.getNeighbour v 3 with
				| None -> ()(*Impossible*)
				| Some(u,p)->
					(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 0 with
						|None ->
							MyGraphs.addVertex ng (MyGraphs.createVertex u);
							MyGraphs.addEdge ng (idv,4) (u,4);
							MyGraphs.setState ng idv Rule.CYAN
						|Some(w,pp)->
							MyGraphs.addVertex ng (MyGraphs.createVertex w);
							MyGraphs.addEdge ng (idv,3) (w,3);
							MyGraphs.setState ng idv Rule.BLEU
					);
			);
		|CYAN ->(*BALLE EN MOUVEMENT RETOUR*)
			(match MyGraphs.getNeighbour v 4 with
				| None ->
					(match MyGraphs.getNeighbour v 3 with
						|None -> ()(*Impossible*)
						|Some(u,p) ->
							(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 2 with
								| None ->
									(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 0 with
										|None->()(*Impossible*)
										|Some(w,pp)->
											MyGraphs.addVertex ng (MyGraphs.createVertex w);
											MyGraphs.addEdge ng (idv,3) (w,3);
											MyGraphs.setState ng idv Rule.CYAN
									);
								| Some(w,pp) ->
									(match MyGraphs.getState g w with
										|JAUNE ->
											MyGraphs.addVertex ng (MyGraphs.createVertex u);
											MyGraphs.addEdge ng (idv,3) (u,3);
											MyGraphs.setState ng idv Rule.BLANC
										|_->()(*Impossible*)
									);
							);
					);
				|Some(u,p) ->
					MyGraphs.setState ng idv Rule.CYAN;
					(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 1 with
						| None ->
							MyGraphs.addVertex ng (MyGraphs.createVertex u);
							MyGraphs.addEdge ng (idv,3) (u,3);
						| Some(w,pp) ->
							MyGraphs.addVertex ng (MyGraphs.createVertex w);
							MyGraphs.addEdge ng (idv,4) (w,4);
					);
			);
		|NOIR->(*FIXE*)
			MyGraphs.setState ng idv Rule.NOIR;
			(match MyGraphs.getNeighbour v 3 with
				|None -> ()(*Impossible*)
				|Some(u,p)->
					MyGraphs.addVertex ng (MyGraphs.createVertex u);
					MyGraphs.addEdge ng (idv,3) (u,3)
			);
		|JAUNE->(*IMMOBILE*)
			(match MyGraphs.getNeighbour v 2 with
				|None -> ()(*Impossible*)
				|Some(u,p)->
					MyGraphs.addVertex ng (MyGraphs.createVertex u);
					MyGraphs.addEdge ng (idv,2) (u,2);
					(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 3 with
						|None ->
							MyGraphs.setState ng idv Rule.JAUNE
						|Some(w,pp)->
							(match MyGraphs.getState g w with
								| BLANC ->
									MyGraphs.setState ng idv Rule.ORANGE
								| _ ->
									MyGraphs.setState ng idv Rule.JAUNE
							);
					);
			);
		|ORANGE->(*EN ARRIERE*)
			(match MyGraphs.getNeighbour v 2 with
			|None -> ()(*Impossible*)
			|Some(u,p)->
				(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 1 with
				|None->
					MyGraphs.addVertex ng (MyGraphs.createVertex u);
					MyGraphs.addEdge ng (idv,2) (u,2);
					MyGraphs.setState ng idv Rule.ROUGE
				|Some(w,pp)->
					MyGraphs.addVertex ng (MyGraphs.createVertex w);
					MyGraphs.addEdge ng (idv,2) (w,2);
					MyGraphs.setState ng idv Rule.ORANGE
				);
			);
		|ROUGE->(*EN AVANT*)
			(match MyGraphs.getNeighbour v 2 with
			|None -> ()(*Impossible*)
			|Some(u,p)->
				(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 3 with
				|None->
					(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 0 with
					|None->()(*Impossible*)
					|Some(w,pp)->
						MyGraphs.addVertex ng (MyGraphs.createVertex w);
						MyGraphs.addEdge ng (idv,2) (w,2);
						MyGraphs.setState ng idv Rule.ROUGE
					);
				|Some(w,pp)->
					(match MyGraphs.getState g w with
					|BLANC->
						MyGraphs.addVertex ng (MyGraphs.createVertex u);
						MyGraphs.addEdge ng (idv,2) (u,2);
						MyGraphs.setState ng idv Rule.JAUNE
					|_->()(*Impossible ?*)
					);
				);
			);
		|_->()(*Inutilisé*)
	);(*print_string (idv^" ok\n");flush_all ();*)
	(*retour ->*)ng;;