open Graph
open Rule

(** J'utilise Graph.Make pour créer mon type de graphes dont les sommets ont MyStates comme états internes **)
module MyGraphs = Graph.Make(Rule.MyStates)

(*Situation initiale pour Moving Head*)
let init_graph n =
	let (r,np) = n in
	let ng=MyGraphs.create () in
	for i=0 to ((np*r)-1) do
		let nom = ("C"^(string_of_int i)) in
		let nom2 = ("G"^(string_of_int i)) in
		MyGraphs.addVertex ng (MyGraphs.createVertex nom);
		MyGraphs.setState ng nom CYAN;
		MyGraphs.addVertex ng (MyGraphs.createVertex nom2);
		MyGraphs.setState ng nom2 BLANC;
		MyGraphs.addEdge ng (nom,3) (nom2,3);
	done;
	for i=0 to ((np*r)-1) do
		let nom = ("C"^(string_of_int i)) in
		let suiv = ("C"^(string_of_int ((i+1) mod (np*r)))) in
		MyGraphs.addEdge ng (nom,0) (suiv,1)
	done;
	for i = 0 to (np-1) do
		let nom = ("P"^(string_of_int i)) in
		let nomAxe = ("M"^(string_of_int i)) in
		MyGraphs.addVertex ng (MyGraphs.createVertex nom);
		MyGraphs.addVertex ng (MyGraphs.createVertex nomAxe);
		MyGraphs.setState ng nom ROUGE;
		MyGraphs.setState ng nomAxe ORANGE;
		MyGraphs.addEdge ng (("C"^(string_of_int (r*i))),2) (nom,2);
		MyGraphs.addEdge ng (nomAxe,3) (nom,3);
	done;
	for i = 0 to (np-1) do
		let nom = ("M"^(string_of_int i)) in
		let suiv = ("M"^(string_of_int ((i+1) mod (np)))) in
		MyGraphs.addEdge ng (nom,0) (suiv,1)
	done;
	(*retour -> *)ng;;

let cgd_rule v g =
	let ng=MyGraphs.create () in
	let idv = fst v in
	MyGraphs.addVertex ng (MyGraphs.createVertex idv);
	(*DEBUG*)MyGraphs.setState ng idv MARRON;
	(match MyGraphs.getState g idv with
		|CYAN->
			MyGraphs.setState ng idv CYAN;
			(match MyGraphs.getNeighbour v 0 with
				|None->()(*Impossible*)
				|Some(a,ap)->
					(match MyGraphs.getNeighbour v 1 with
						|None->()(*Impossible*)
						|Some(b,bp)->
							MyGraphs.addVertex ng (MyGraphs.createVertex a);
							MyGraphs.addVertex ng (MyGraphs.createVertex b);
							MyGraphs.addEdge ng (idv,0) (a,1);
							MyGraphs.addEdge ng (idv,1) (b,0);
					);				
			);
		|BLANC->
			(match MyGraphs.getNeighbour v 3 with
				|None->()(*Impossible*)
				|Some(c,cp)->
					(match MyGraphs.getNeighbour (MyGraphs.getVertex g c) 2 with
						|None->
							MyGraphs.setState ng idv BLANC;
							MyGraphs.addVertex ng (MyGraphs.createVertex c);
							MyGraphs.addEdge ng (idv,3) (c,3);
						|Some(p,pp)->
							(match MyGraphs.getState g p with
							|ROUGE->
								MyGraphs.setState ng idv JAUNE;
								MyGraphs.addVertex ng (MyGraphs.createVertex p);
								MyGraphs.addEdge ng (idv,0) (p,1);
							|BLEU->
								()
							|_->()(*Impossible*)
							);
					);					
			);
		|JAUNE->
			(match MyGraphs.getNeighbour v 0 with
				|None->()(*Impossible*)
				|Some(u,up)->
					(match MyGraphs.getState g u with
						|JAUNE->
							(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 0 with
								|None->()(*Impossible*)
								|Some(w,wp)->
									(match MyGraphs.getState g w with
										|JAUNE->
											MyGraphs.setState ng idv JAUNE;
											MyGraphs.addVertex ng (MyGraphs.createVertex u);
											MyGraphs.addEdge ng (idv,0) (u,1);
										|ROUGE->
											(match MyGraphs.getNeighbour (MyGraphs.getVertex g w) 2 with
												|None->()(*Impossible*)
												|Some(c,cp)->
													(match MyGraphs.getNeighbour (MyGraphs.getVertex g c) 3 with
														|None->
															MyGraphs.setState ng idv JAUNE;
															MyGraphs.addVertex ng (MyGraphs.createVertex w);
															MyGraphs.addEdge ng (idv,0) (w,1);
														|Some(g,gp)->
															MyGraphs.setState ng idv JAUNE;
															MyGraphs.addVertex ng (MyGraphs.createVertex u);
															MyGraphs.addEdge ng (idv,0) (u,1);
													);	
											);
										|_->()
									);
							);
						|ROUGE->
							(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 2 with
								|None->()(*Impossible*)
								|Some(c,cp)->
									(match MyGraphs.getNeighbour (MyGraphs.getVertex g c) 3 with
										|None->
											MyGraphs.setState ng idv BLANC;
											MyGraphs.addVertex ng (MyGraphs.createVertex c);
											MyGraphs.addEdge ng (idv,3) (c,3);
										|Some(g,gp)->
											MyGraphs.setState ng idv JAUNE;
											MyGraphs.addVertex ng (MyGraphs.createVertex g);
											MyGraphs.addEdge ng (idv,0) (g,1);
									);
							);
						|_->()(*Impossible*)
					);
			);
		|ROUGE->
			MyGraphs.setState ng idv ROUGE;
			(match MyGraphs.getNeighbour v 3 with
				|None->()(*Impossible*)
				|Some(m,mp)->
					MyGraphs.addVertex ng (MyGraphs.createVertex m);
					MyGraphs.addEdge ng (idv,3) (m,mp);
					(match MyGraphs.getNeighbour v 2 with
						|None->()(*Impossible*)
						|Some(c,cp)->
							(match MyGraphs.getNeighbour (MyGraphs.getVertex g c) 0 with
								|None->()(*Impossible*)
								|Some(c2,c2p)->
									MyGraphs.addVertex ng (MyGraphs.createVertex c2);
									MyGraphs.addEdge ng (idv,2) (c2,2);
							);
					);
			);
		|ORANGE->
			MyGraphs.setState ng idv ORANGE;
			(match MyGraphs.getNeighbour v 0 with
			|None->()(*Impossible*)
			|Some(m,mp)->
				MyGraphs.addVertex ng (MyGraphs.createVertex m);
				MyGraphs.addEdge ng (idv,0) (m,1);
			);
		|_->()
	);
	ng;;
	



	(*
	print_string "coucou\n"; flush_all ();
	*)


(*ANCIENNE VERSION MONO-AXE
open Graph
open Rule

(** J'utilise Graph.Make pour créer mon type de graphes dont les sommets ont MyStates comme états internes **)
module MyGraphs = Graph.Make(Rule.MyStates)

(*Situation initiale pour Moving Head*)
let init_graph n =
	let (r,np) = n in
	let ng=MyGraphs.create () in
	for i=0 to ((np*r)-1) do
		let nom = ("C"^(string_of_int i)) in
		let nom2 = ("G"^(string_of_int i)) in
		MyGraphs.addVertex ng (MyGraphs.createVertex nom);
		MyGraphs.setState ng nom CYAN;
		MyGraphs.addVertex ng (MyGraphs.createVertex nom2);
		MyGraphs.setState ng nom2 BLANC;
		MyGraphs.addEdge ng (nom,3) (nom2,3);
	done;
	for i=0 to ((np*r)-1) do
		let nom = ("C"^(string_of_int i)) in
		let suiv = ("C"^(string_of_int ((i+1) mod (np*r)))) in
		MyGraphs.addEdge ng (nom,0) (suiv,1)
	done;
	MyGraphs.addVertex ng (MyGraphs.createVertex "M");
	MyGraphs.setState ng "M" ORANGE;
	for i = 0 to (np-1) do
		let nom = ("P"^(string_of_int i)) in
		MyGraphs.addVertex ng (MyGraphs.createVertex nom);
		MyGraphs.setState ng nom ROUGE;
		MyGraphs.addEdge ng (("C"^(string_of_int (r*i))),2) (nom,2);
		MyGraphs.addEdge ng ("M",i) (nom,3);
	done;
	(*retour -> *)ng;;

let cgd_rule v g =
	let ng=MyGraphs.create () in
	let idv = fst v in
	MyGraphs.addVertex ng (MyGraphs.createVertex idv);
	(*DEBUG*)MyGraphs.setState ng idv MARRON;
	(match MyGraphs.getState g idv with
		|CYAN->
			MyGraphs.setState ng idv CYAN;
			(match MyGraphs.getNeighbour v 0 with
				|None->()(*Impossible*)
				|Some(a,ap)->
					(match MyGraphs.getNeighbour v 1 with
						|None->()(*Impossible*)
						|Some(b,bp)->
							MyGraphs.addVertex ng (MyGraphs.createVertex a);
							MyGraphs.addVertex ng (MyGraphs.createVertex b);
							MyGraphs.addEdge ng (idv,0) (a,1);
							MyGraphs.addEdge ng (idv,1) (b,0);
					);				
			);
		|BLANC->
			(match MyGraphs.getNeighbour v 3 with
				|None->()(*Impossible*)
				|Some(c,cp)->
					(match MyGraphs.getNeighbour (MyGraphs.getVertex g c) 2 with
						|None->
							MyGraphs.setState ng idv BLANC;
							MyGraphs.addVertex ng (MyGraphs.createVertex c);
							MyGraphs.addEdge ng (idv,3) (c,3);
						|Some(p,pp)->
							(match MyGraphs.getState g p with
							|ROUGE->
								MyGraphs.setState ng idv JAUNE;
								MyGraphs.addVertex ng (MyGraphs.createVertex p);
								MyGraphs.addEdge ng (idv,0) (p,1);
							|BLEU->
								()
							|_->()(*Impossible*)
							);
					);					
			);
		|JAUNE->
			(match MyGraphs.getNeighbour v 0 with
				|None->()(*Impossible*)
				|Some(u,up)->
					(match MyGraphs.getState g u with
						|JAUNE->
							(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 0 with
								|None->()(*Impossible*)
								|Some(w,wp)->
									(match MyGraphs.getState g w with
										|JAUNE->
											MyGraphs.setState ng idv JAUNE;
											MyGraphs.addVertex ng (MyGraphs.createVertex u);
											MyGraphs.addEdge ng (idv,0) (u,1);
										|ROUGE->
											(match MyGraphs.getNeighbour (MyGraphs.getVertex g w) 2 with
												|None->()(*Impossible*)
												|Some(c,cp)->
													(match MyGraphs.getNeighbour (MyGraphs.getVertex g c) 3 with
														|None->
															MyGraphs.setState ng idv JAUNE;
															MyGraphs.addVertex ng (MyGraphs.createVertex w);
															MyGraphs.addEdge ng (idv,0) (w,1);
														|Some(g,gp)->
															MyGraphs.setState ng idv JAUNE;
															MyGraphs.addVertex ng (MyGraphs.createVertex u);
															MyGraphs.addEdge ng (idv,0) (u,1);
													);	
											);
										|_->()
									);
							);
						|ROUGE->
							(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 2 with
								|None->()(*Impossible*)
								|Some(c,cp)->
									(match MyGraphs.getNeighbour (MyGraphs.getVertex g c) 3 with
										|None->
											MyGraphs.setState ng idv BLANC;
											MyGraphs.addVertex ng (MyGraphs.createVertex c);
											MyGraphs.addEdge ng (idv,3) (c,3);
										|Some(g,gp)->
											MyGraphs.setState ng idv JAUNE;
											MyGraphs.addVertex ng (MyGraphs.createVertex g);
											MyGraphs.addEdge ng (idv,0) (g,1);
									);
							);
						|_->()(*Impossible*)
					);
			);
		|ROUGE->
			MyGraphs.setState ng idv ROUGE;
			(match MyGraphs.getNeighbour v 3 with
				|None->()(*Impossible*)
				|Some(m,mp)->
					MyGraphs.addVertex ng (MyGraphs.createVertex m);
					MyGraphs.addEdge ng (idv,3) (m,mp);
					(match MyGraphs.getNeighbour v 2 with
						|None->()(*Impossible*)
						|Some(c,cp)->
							(match MyGraphs.getNeighbour (MyGraphs.getVertex g c) 0 with
								|None->()(*Impossible*)
								|Some(c2,c2p)->
									MyGraphs.addVertex ng (MyGraphs.createVertex c2);
									MyGraphs.addEdge ng (idv,2) (c2,2);
							);
					);
			);
		|ORANGE->
			MyGraphs.setState ng idv ORANGE;
		|_->()
	);
	ng;;
	



	(*
	print_string "coucou\n"; flush_all ();
	*)
*)