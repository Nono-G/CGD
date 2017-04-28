open Graph
open Rule


module MyGraphs = Graph.Make(Rule.MyStates)

let init_graph n =
	let (an,mn) = n in
	let ng=MyGraphs.create () in
	for i=0 to an-1 do
		let a = ("A"^(string_of_int i)) in
		let g = ("G"^(string_of_int i)) in
		let d = ("D"^(string_of_int i)) in
		MyGraphs.addVertex ng (MyGraphs.createVertex a);
		MyGraphs.addVertex ng (MyGraphs.createVertex g);
		MyGraphs.addVertex ng (MyGraphs.createVertex d);
		MyGraphs.setState ng a BLANC;
		MyGraphs.setState ng g BORDEAUX;
		MyGraphs.setState ng d BORDEAUX;
	done;
	for i=0 to (an-2) do
		let a = ("A"^(string_of_int i)) in
		let a2 = ("A"^(string_of_int (i+1))) in
		let g = ("G"^(string_of_int i)) in
		let d = ("D"^(string_of_int i)) in
		MyGraphs.addEdge ng (a,2) (g,0);
		MyGraphs.addEdge ng (a,3) (d,0);
		MyGraphs.addEdge ng (a,0) (a2,1);
	done;
	MyGraphs.addEdge ng (("A"^(string_of_int (an-1))),2) (("G"^(string_of_int (an-1))),0);
	MyGraphs.addEdge ng (("A"^(string_of_int (an-1))),3) (("D"^(string_of_int (an-1))),0);
	for i=0 to mn-1 do
		let m = ("M"^(string_of_int i)) in
		MyGraphs.addVertex ng (MyGraphs.createVertex m);
		MyGraphs.setState ng m MARRON;
	done;
	for i=0 to (mn-2) do
		let m = ("M"^(string_of_int i)) in
		let m2 = ("M"^(string_of_int (i+1))) in
		MyGraphs.addEdge ng (m,0) (m2,1);
	done;
	MyGraphs.addEdge ng ("M0",4) ("A0",4);

	(*retour -> *)ng;;

(*Moving Head : GCD un sommet 'tête' se déplace le long d'une (0;1)-chaîne 'ruban'*)
let cgd_rule v g =
	let ng = MyGraphs.create() in (*Création d'un nouveau graphe*)
	let idv = fst v in
	print_string idv;flush_all ();
	MyGraphs.addVertex ng (MyGraphs.createVertex idv);(*On va conserver les mêmes sommets*)
	MyGraphs.setState ng idv FUCHSIA;
	(match MyGraphs.getState g idv with
		|BORDEAUX->
			(match MyGraphs.getNeighbour v 0 with
				|None->()(*Impossible*)
				|Some(a,ap)->
					MyGraphs.addVertex ng (MyGraphs.createVertex a);
					MyGraphs.addEdge ng (idv,0) (a,ap);
					(match MyGraphs.getNeighbour (MyGraphs.getVertex g a) 0 with
						|None->
							MyGraphs.setState ng idv ORANGE;
						|Some(a0,a0p)->
							(match MyGraphs.getNeighbour (MyGraphs.getVertex g a0) 3 with
								|None->()(*Impossible ?*)
								|Some(a0d,a0dp)->
									(match MyGraphs.getState g a0d with
										|ORANGE->
											MyGraphs.setState ng idv ORANGE;
										|_->
											MyGraphs.setState ng idv BORDEAUX;
									);
							);
					);
			);
		|BLANC->
			(match MyGraphs.getNeighbour v 2 with
				|None->()(*Impossible*)
				|Some(gauche,gp)->
					(match MyGraphs.getNeighbour v 3 with
						|None->()(*Impossible*)
						|Some(d,dp)->
							(match MyGraphs.getState g gauche  with
								|ORANGE->
									MyGraphs.setState ng idv NOIR;
								|_->
									MyGraphs.setState ng idv BLANC;
									MyGraphs.addVertex ng (MyGraphs.createVertex gauche);
									MyGraphs.addVertex ng (MyGraphs.createVertex d);
									MyGraphs.addEdge ng (idv,2) (gauche,0);
									MyGraphs.addEdge ng (idv,3) (d,0);
							);
							(match MyGraphs.getNeighbour v 0 with
								|None->()
								|Some(a0,a0p)->
									MyGraphs.addVertex ng (MyGraphs.createVertex a0);
									MyGraphs.addEdge ng (idv,0) (a0,1);
							);
							(match MyGraphs.getNeighbour v 1 with
								|None->()
								|Some(a1,a1p)->
									MyGraphs.addVertex ng (MyGraphs.createVertex a1);
									MyGraphs.addEdge ng (idv,1) (a1,0);
							);							
					);
			);
		|ORANGE->
			(match MyGraphs.getNeighbour v 0 with
				|None->()(*Impossible*)
				|Some(a,ap)->
					(match MyGraphs.getNeighbour (MyGraphs.getVertex g a) 1 with
						|None->
							MyGraphs.addVertex ng (MyGraphs.createVertex a);
							MyGraphs.addEdge ng (idv,0) (a,ap);
							MyGraphs.setState ng idv CIEL;
						|Some(a1,a1p)->
							(match MyGraphs.getNeighbour (MyGraphs.getVertex g a1) 2 with
								|None->()(*Impossible*)
								|Some(a1g,a1gp)->
									(match MyGraphs.getNeighbour (MyGraphs.getVertex g a1) 3 with
										|None->()(*Impossible*)
										|Some(a1d,a1dp)->
											MyGraphs.setState ng idv GRIS;
											(match ap with
												|2->(*GAUCHE*)
													MyGraphs.addVertex ng (MyGraphs.createVertex a1g);
													MyGraphs.addEdge ng (idv,0) (a1g,1);
												|_->(*DROITE*)
													MyGraphs.addVertex ng (MyGraphs.createVertex a1d);
													MyGraphs.addEdge ng (idv,0) (a1d,1);
											);
									);
							);
					);				
			);
		|GRIS->
			(match MyGraphs.getNeighbour v 0 with
				|None->()(*Impossible*)
				|Some(u,up)->
					(match MyGraphs.getState g u with
						|CIEL->
							MyGraphs.setState ng idv CIEL;
							(match MyGraphs.getNeighbour (MyGraphs.getVertex g u) 0 with
								|None->()(*Impossible*)
								|Some(a,ap)->
									(match MyGraphs.getNeighbour (MyGraphs.getVertex g a) 0 with
										|None->()
										|Some(a0,a0p)->
											MyGraphs.addVertex ng (MyGraphs.createVertex a0);
											MyGraphs.addEdge ng (idv,0) (a0,ap);										
									);
							);
						|_->
							MyGraphs.setState ng idv GRIS;
							MyGraphs.addVertex ng (MyGraphs.createVertex u);
							MyGraphs.addEdge ng (idv,0) (u,1);
					);
			);
		|NOIR->
			(match MyGraphs.getNeighbour v 1 with
				|None->()
				|Some(u,up)->
					MyGraphs.addVertex ng (MyGraphs.createVertex u);
					MyGraphs.addEdge ng (idv,1) (u,0);
			);
			(match MyGraphs.getNeighbour v 3 with
				|None->
					MyGraphs.setState ng idv NOIR;
				|Some(d,dp)->
					MyGraphs.setState ng idv BLANC;
			);
		|CIEL->
			(match MyGraphs.getNeighbour v 0 with
				|None->()(*Impossible*)
				|Some(a,ap)->
					MyGraphs.addVertex ng (MyGraphs.createVertex a);
					MyGraphs.addEdge ng (idv,0) (a,ap);
					MyGraphs.setState ng idv BORDEAUX;
			);
		|MARRON->
			MyGraphs.setState ng idv MARRON;
			(match MyGraphs.getNeighbour v 4 with
				|None->()
				|Some(a,ap)->
					MyGraphs.addVertex ng (MyGraphs.createVertex a);
					MyGraphs.addEdge ng (idv,4) (a,4);
			);
			(match MyGraphs.getNeighbour v 0 with
				|None->()
				|Some(m0,m0p)->
					MyGraphs.addVertex ng (MyGraphs.createVertex m0);
					MyGraphs.addEdge ng (idv,0) (m0,1);
			);
		|_->()
	);print_string " OK\n";flush_all ();
(**retour->*)ng;;
	(*
(match MyGraphs.getNeighbour v 0 with
	|None->()(*Impossible*)
	|Some(a,ap)->
);	
	*)