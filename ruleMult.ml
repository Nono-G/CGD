open Graph
open Rule

module Gr = Graph.Make(Rule.MyStates)

let init_graph p =
	let ng=Gr.create() in
	let (a,b,c) = p in
	for i=0 to c do
		let nom = ("R"^(string_of_int i)) in
		Gr.addVertex ng (Gr.createVertex nom);
		Gr.setState ng nom BLANC
	done;
	for i=0 to (c-1) do
		let nom = ("R"^(string_of_int i)) in
		let suiv = ("R"^(string_of_int (i+1))) in
		Gr.addEdge ng (nom,0) (suiv,1)
	done;
	for i=0 to (b-1) do
		let nom = ("B"^(string_of_int i)) in
		Gr.addVertex ng (Gr.createVertex nom);
		Gr.setState ng nom NOIR
	done;
	for i=0 to (b-1) do
		let nom = ("B"^(string_of_int i)) in
		let suiv = ("B"^(string_of_int ((i+1) mod b))) in
		Gr.addEdge ng (nom,0) (suiv,1)
	done;
	for i=0 to (a-1) do
		let nom = ("A"^(string_of_int i)) in
		Gr.addVertex ng (Gr.createVertex nom);
		Gr.setState ng nom NOIR
	done;
	for i=0 to (a-1) do
		let nom = ("A"^(string_of_int i)) in
		let suiv = ("A"^(string_of_int ((i+1) mod a))) in
		Gr.addEdge ng (nom,0) (suiv,1)
	done;
	Gr.addVertex ng (Gr.createVertex "A");
	Gr.addVertex ng (Gr.createVertex "B");
	Gr.setState ng "A" ROUGE;
	Gr.setState ng "B" BLEU;
	Gr.addEdge ng ("A",2) ("A0",2);
	Gr.addEdge ng ("B",2) ("B0",2);
	Gr.addEdge ng ("A0",3) ("R0",2);
	Gr.addEdge ng ("B0",3) ("R0",3);
	Gr.setState ng ("R"^(string_of_int c)) VERT;
	ng;;


let cgd_rule v g =
	let ng=Gr.create() in
	let idv = fst v in
	Gr.addVertex ng (Gr.createVertex idv);
	Gr.setState ng idv MARRON;
	(match Gr.getState g idv with
		|ROUGE ->
			(match Gr.getNeighbour v 2 with
				|None -> ()(*Impossible*)
				|Some(u,p)->
					(match Gr.getState g u with
						|SAPIN ->
							Gr.setState ng idv BORDEAUX;
							Gr.addVertex ng (Gr.createVertex u);
							Gr.addEdge ng (idv,2) (u,2)
						| _ ->
							Gr.setState ng idv ROUGE;
							(match Gr.getNeighbour (Gr.getVertex g u) 0 with
								|None ->()(*Impossible*)
								|Some(w,pp)->
									Gr.addVertex ng (Gr.createVertex w);
									Gr.addEdge ng (idv,2) (w,2)
							);
					);
			);
		|BLEU ->
			(match Gr.getNeighbour v 2 with
				|None -> ()(*Impossible*)
				|Some(u,p)->
					(match Gr.getState g u with
						|SAPIN ->
							Gr.setState ng idv MARINE;
							Gr.addVertex ng (Gr.createVertex u);
							Gr.addEdge ng (idv,2) (u,2)
						| _ ->
							Gr.setState ng idv BLEU;
							(match Gr.getNeighbour (Gr.getVertex g u) 0 with
								|None ->()(*Impossible*)
								|Some(w,pp)->
									Gr.addVertex ng (Gr.createVertex w);
									Gr.addEdge ng (idv,2) (w,2)
							);
					);
			);
		|BORDEAUX ->
			Gr.setState ng idv BORDEAUX;
			(match Gr.getNeighbour v 2 with
				|None->()(*Impossible*)
				|Some(u,pu)->
					Gr.addVertex ng (Gr.createVertex u);
					Gr.addEdge ng (idv,2) (u,2)
			);
		|MARINE ->
			Gr.setState ng idv MARINE;
			(match Gr.getNeighbour v 2 with
				|None->()(*Impossible*)
				|Some(u,pu)->
					Gr.addVertex ng (Gr.createVertex u);
					Gr.addEdge ng (idv,2) (u,2)
			);
		|NOIR ->
			(match Gr.getNeighbour v 3 with
				|None->
					Gr.setState ng idv NOIR;
				|Some(u,up)->
					(match Gr.getState g u with
						|VERT->
							Gr.setState ng idv SAPIN
						|_->
							Gr.setState ng idv NOIR;
					);
			);
			(match Gr.getNeighbour v 0 with
				|None->()(*Impossible*)
				|Some(u,p)->
					Gr.addVertex ng (Gr.createVertex u);
					Gr.addEdge ng (idv,0) (u,1)
			);
			(match Gr.getNeighbour v 1 with
				|None->()(*Impossible*)
				|Some(w,p)->
					Gr.addVertex ng (Gr.createVertex w);
					Gr.addEdge ng (idv,1) (w,0)
			);
		|SAPIN ->
			Gr.setState ng idv SAPIN;
			(match Gr.getNeighbour v 0 with
				|None->()(*Impossible*)
				|Some(u,p)->
					Gr.addVertex ng (Gr.createVertex u);
					Gr.addEdge ng (idv,0) (u,1)
			);
			(match Gr.getNeighbour v 1 with
				|None->()(*Impossible*)
				|Some(w,p)->
					Gr.addVertex ng (Gr.createVertex w);
					Gr.addEdge ng (idv,1) (w,0)
			);
			(match Gr.getNeighbour v 2 with
				|None->()
				|Some(u,p)->
					Gr.addVertex ng (Gr.createVertex u);
					Gr.addEdge ng (idv,2) (u,2)
			);
			(match Gr.getNeighbour v 3 with
				|None->()
				|Some(u,p)->
					Gr.addVertex ng (Gr.createVertex u);
					Gr.addEdge ng (idv,3) (u,3)
			);
		|VERT ->
			Gr.setState ng idv VERT;
			(match Gr.getNeighbour v 1 with
				|None -> () (*Impossible*)
				|Some(u,up)->
					Gr.addVertex ng (Gr.createVertex u);
					Gr.addEdge ng (idv,1) (u,0);
					(match Gr.getNeighbour v 2 with
						|None->
							(match Gr.getNeighbour (Gr.getVertex g u) 2 with
								|None->()
								|Some(a,pp)->
									Gr.addVertex ng (Gr.createVertex a);
									Gr.addEdge ng (idv,2) (a,3);
									(match Gr.getNeighbour (Gr.getVertex g u) 3 with
										|None->()(*Impossible*)
										|Some(b,ppp)->
											Gr.addVertex ng (Gr.createVertex b);
											Gr.addEdge ng (idv,3) (b,3);
									);									
							);
						|Some(a,ap)->
							(match Gr.getNeighbour v 3 with
								|None->()(*Impossible*)
								|Some(b,bp)->
									Gr.addVertex ng (Gr.createVertex a);
									Gr.addEdge ng (idv,2) (a,3);
									Gr.addVertex ng (Gr.createVertex b);
									Gr.addEdge ng (idv,3) (b,3);
							);
					);
					(*
					(match Gr.getNeighbour (Gr.getVertex g u) 2 with
						|None->()
						|Some(a,pp)->
							Gr.addVertex ng (Gr.createVertex a);
							Gr.addEdge ng (idv,2) (a,2);
					);
					(match Gr.getNeighbour (Gr.getVertex g u) 3 with
						|None->()
						|Some(b,ppp)->
							Gr.addVertex ng (Gr.createVertex b);
							Gr.addEdge ng (idv,3) (b,3);
					);
				*)
			);
		|BLANC ->
			(match Gr.getNeighbour v 1 with
				|None -> () (*v est Tête de la chaine*)
				|Some(u,p)->
					Gr.addVertex ng (Gr.createVertex u);
					Gr.addEdge ng (idv,1) (u,0);
					(match Gr.getNeighbour (Gr.getVertex g u) 2 with
						|None->()
						|Some(a,pp)->
							Gr.addVertex ng (Gr.createVertex a);
							Gr.addEdge ng (idv,2) (a,3);
					);
					(match Gr.getNeighbour (Gr.getVertex g u) 3 with
						|None->()
						|Some(b,ppp)->
							Gr.addVertex ng (Gr.createVertex b);
							Gr.addEdge ng (idv,3) (b,3);
					);
			);
			(match Gr.getNeighbour v 0 with
				|None->()(*Impossible*)
				|Some(w,pw)->
					Gr.addVertex ng (Gr.createVertex w);
					Gr.addEdge ng (idv,0) (w,1);
			);
			(match Gr.getNeighbour v 2 with
				|None ->
					Gr.setState ng idv BLANC;
				|Some(a,pa)->
					(match Gr.getNeighbour v 3 with
						|None->()(*Impossible*)
						|Some(b,pb)->
							(match Gr.getNeighbour (Gr.getVertex g a) 2 with
								|None->
									(match Gr.getNeighbour (Gr.getVertex g b) 2 with
										|None->
											Gr.setState ng idv GRIS;
										|Some(tb,ptb)->
											Gr.setState ng idv CYAN;	
									);
								|Some(ta,pta)->
									(match Gr.getNeighbour (Gr.getVertex g b) 2 with
										|None->
											Gr.setState ng idv ORANGE;
										|Some(tb,ptb)->
											Gr.setState ng idv FUCHSIA;	
									);
							);
					);
			);
		|GRIS
		|ORANGE
		|CYAN
		|FUCHSIA ->
			Gr.setState ng idv (Gr.getState g idv);
			(match Gr.getNeighbour v 0 with
				|None->()(*Queue de la chaine, faudrait faire un truc pour arrêter la machine*)
				|Some(w,pw)->
					Gr.addVertex ng (Gr.createVertex w);
					Gr.addEdge ng (idv,0) (w,1);
			);
			(match Gr.getNeighbour v 1 with
				|None->()(*Queue de la chaine, faudrait faire un truc pour arrêter la machine*)
				|Some(w,pw)->
					Gr.addVertex ng (Gr.createVertex w);
					Gr.addEdge ng (idv,1) (w,0);
			);
		|_->()
	);
	ng;;