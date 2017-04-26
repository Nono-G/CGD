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
let init_graph (n:int) =
	let p = 20 in
	let ng=MyGraphs.create () in
	for i = 0 to (n-1) do
		let nom = ("N"^(string_of_int i)) in
		MyGraphs.addVertex ng (MyGraphs.createVertex nom);
		(match i mod p with
			|0 ->
				MyGraphs.setState ng nom VERT
			|_ ->
				MyGraphs.setState ng nom NOIR;
		);
	done;
	for i = 0 to (n-1) do
		let succ = ((i+1)mod n) in
		let nom = ("N"^(string_of_int i)) in
		let nom2 = ("N"^(string_of_int succ)) in
		MyGraphs.addEdge ng (nom,0) (nom2,1);
	done;
	let y = if(n mod p)=0
		then (n/p)-1
		else (n/p)
	in
	for i = 0 to y do
		let x = p*i in
		let succ = p*(i+1) in
		(if succ < n then
			MyGraphs.addEdge ng (("N"^(string_of_int x)),2) (("N"^(string_of_int succ)),3)
		else
			MyGraphs.addEdge ng (("N"^(string_of_int x)),2) ("N0",3)
		);
	done;
	MyGraphs.setState ng "N380" ROUGE;
	MyGraphs.setState ng "N180" ROUGE;
	MyGraphs.setState ng "N240" ROUGE;
	MyGraphs.setState ng "N320" ROUGE;
	MyGraphs.setState ng "N40" BLEU;
	MyGraphs.setState ng "N80" BLEU;
	MyGraphs.setState ng "N120" BLEU;
	(*retour -> *)ng;;

let rec bmDist idv idu g =
	if (idv=idu) then 0
	else
		let Some(idw,p)=MyGraphs.getNeighbour (MyGraphs.getVertex g idv) 1 in
		1+(bmDist idw idu g);;

let rec bmGetNPrev n idv g =
	if (n=0) then idv
	else
		let Some(idw,p)=MyGraphs.getNeighbour (MyGraphs.getVertex g idv) 1 in
		bmGetNPrev (n-1) idw g;;

let bmPrevMiddle idv g =
	let prevCircle =
		(match MyGraphs.getNeighbour (MyGraphs.getVertex g idv) 3 with
		| None -> ""(*Impossible ?*)
		| Some(idu,p) -> idu
	)in
	let x = bmDist idv prevCircle g in
	(prevCircle,(bmGetNPrev (x/2) idv g),idv);;

(*USE ONLY DURING SCATTER PHASE !*)
let rec circlePrev idv g =
	match MyGraphs.getState g idv with
	|BLANC->
		let Some(idu,p)=MyGraphs.getNeighbour (MyGraphs.getVertex g idv) 1 in
		circlePrev idu g
	|_-> idv;;

(*USE ONLY DURING SCATTER PHASE !*)
let rec circleNext idv g =
	match MyGraphs.getState g idv with
	|BLANC-> 
		let Some(idu,p)=MyGraphs.getNeighbour (MyGraphs.getVertex g idv) 0 in
		circleNext idu g
	|_-> idv;;

let cgd_rule v g =
	(*print_string (fst v);flush_all();*)
	let ng = MyGraphs.create() in (*Création d'un nouveau graphe*)
	let idv = fst v in
	MyGraphs.addVertex ng (MyGraphs.createVertex idv);(*On va conserver les mêmes sommets*)
	let Some(bmPrev,p1) = MyGraphs.getNeighbour v 1 in
	let Some(bmNext,p2) = MyGraphs.getNeighbour v 0 in
	MyGraphs.setState ng idv MARRON;
	MyGraphs.addVertex ng (MyGraphs.createVertex bmPrev);
	MyGraphs.addVertex ng (MyGraphs.createVertex bmNext);
	MyGraphs.addEdge ng (idv,1) (bmPrev,0);
	MyGraphs.addEdge ng (idv,0) (bmNext,1);
	(match MyGraphs.getState g idv with
	| NOIR (*BLACKMATTER ADVECT*)->
		MyGraphs.setState ng idv BLANC;
	| VERT (*CIRCLE VERTEX ADVECT*)
	| ROUGE (*->P ADVECT*)
	| BLEU (*<-P ADVECT*)
	| VIOLET (*<-PP-> ADVECT*)->
		(match MyGraphs.getNeighbour v 2 with
		| None -> () (*Impossible ?*)
		| Some(a,p1) ->
			(match MyGraphs.getNeighbour v 3 with
			| None -> () (*Impossible ?*)
			| Some(b,p2) ->
				MyGraphs.addVertex ng (MyGraphs.createVertex a);
				MyGraphs.addVertex ng (MyGraphs.createVertex b);
				MyGraphs.addEdge ng (idv,2) (a,3);
				MyGraphs.addEdge ng (idv,3) (b,2);
				(match MyGraphs.getState g a with
				| BLEU | VIOLET ->
					(match MyGraphs.getState g b with
					| ROUGE | VIOLET ->
						MyGraphs.setState ng idv FUCHSIA
					| _ ->
						MyGraphs.setState ng idv CYAN
					);
				| _ ->
					(match MyGraphs.getState g b with
					| ROUGE |VIOLET ->
						MyGraphs.setState ng idv ORANGE
					| _ ->
						MyGraphs.setState ng idv JAUNE
					);
				);
			);
		);
	| BLANC ->(*BLACKMATTER SCATTER*)
		let cn = circleNext idv g in
		let cp = circlePrev idv g in
		(match  MyGraphs.getState g cn with
		| FUCHSIA ->
			(*Vertex 'creation'*)
			let (x,y,z)=bmPrevMiddle cn g in
			(if y=idv then
				(
				MyGraphs.addVertex ng (MyGraphs.createVertex x);
				MyGraphs.addVertex ng (MyGraphs.createVertex z);
				MyGraphs.addEdge ng (idv,3) (x,2);
				MyGraphs.addEdge ng (idv,2) (z,3);
				MyGraphs.setState ng idv BLEU)
			else
				MyGraphs.setState ng idv NOIR
			);
		(*| ORANGE ->
			(match MyGraphs.getState g cp with
			| CYAN ->
				(*Vertex 'destruction'*)
				let (x,y,z)=bmPrevMiddle cn g in
				(if(y=idv)then
					let Some(prevprev,p1) = MyGraphs.getNeighbour (MyGraphs.getVertex g cp) 3 in
					let Some(nextnext,p2) = MyGraphs.getNeighbour (MyGraphs.getVertex g cn) 2 in
					MyGraphs.addVertex ng (MyGraphs.createVertex nextnext);
					MyGraphs.addVertex ng (MyGraphs.createVertex prevprev);
					MyGraphs.addEdge ng (idv,3) (prevprev,2);
					MyGraphs.addEdge ng (idv,2) (nextnext,3);
					MyGraphs.setState ng idv VIOLET
				else
					MyGraphs.setState ng idv NOIR
				);
			| _ ->
				MyGraphs.setState ng idv NOIR
			);*)
		| _ ->
			MyGraphs.setState ng idv NOIR
		);
	| JAUNE ->(*CIRCLE VERTEX SCATTER*)
		(*Si je suis jaune et que mon voisin 2 "a" est cyan et que le voisin 2 "b" de "a" est orange, je me connecte a "b" et je reste vert*)
		(*Si je suis jaune et que mon voisin 2 est fuchsia, je ne me connecte pas a lui*)
		(*Je me connecte toujours a mon voisin 3*)
		MyGraphs.setState ng idv VERT;
		(match MyGraphs.getNeighbour v 3 with
			|None->()(*Impossible ?*)
			|Some(a,p)->
				MyGraphs.addVertex ng (MyGraphs.createVertex a);
				MyGraphs.addEdge ng (idv,3) (a,2);
		);
		(match MyGraphs.getNeighbour v 2 with
			|None->()(*Impossible ?*)
			|Some(a,p)->
				(match MyGraphs.getState g a with
				| CYAN ->
					(match MyGraphs.getNeighbour (MyGraphs.getVertex g a) 2 with
						|None->()(*Impossible ?*)
						|Some(b,pp)->
							(match MyGraphs.getState g b with
								|ORANGE ->
									MyGraphs.addVertex ng (MyGraphs.createVertex b);
									MyGraphs.addEdge ng (idv,2) (b,3)
								| _ -> 
									MyGraphs.addVertex ng (MyGraphs.createVertex a);
									MyGraphs.addEdge ng (idv,2) (a,3)
							);
					);
				| FUCHSIA -> ()
				| _ ->
					MyGraphs.addVertex ng (MyGraphs.createVertex a);
					MyGraphs.addEdge ng (idv,2) (a,3)					
				);
		);
		(*
		let Some(cPrev,p1) = MyGraphs.getNeighbour v 3 in
		let Some(cNext,p2) = MyGraphs.getNeighbour v 2 in
		MyGraphs.setState ng idv VERT;
		(match MyGraphs.getState g cPrev with
		| JAUNE ->
			MyGraphs.addVertex ng (MyGraphs.createVertex cPrev);
			MyGraphs.addEdge ng (idv,3) (cPrev,2)
		| _ -> ()
		);
		(match MyGraphs.getState g cNext with
		| JAUNE ->
			MyGraphs.addVertex ng (MyGraphs.createVertex cNext);
			MyGraphs.addEdge ng (idv,2) (cNext,3)
		| _ -> ()
		);
		*)
	| ORANGE ->(*P-> SCATTER*)
		(*Si je suis orange et que mon voisin 3 (cercle) est cyan, je devient violet et je me relie au voisin 3 de mon voisin 3*)
		(match MyGraphs.getNeighbour v 3 with
			|None->()(*Impossible ?*)
			|Some(a,p)->
				(match MyGraphs.getState g a with
					|CYAN ->
						MyGraphs.setState ng idv VIOLET;
						(match MyGraphs.getNeighbour (MyGraphs.getVertex g a) 3 with
							|None->()(*Impossible ?*)
							|Some(b,pp)->
								MyGraphs.addVertex ng (MyGraphs.createVertex b);
								MyGraphs.addEdge ng (idv,3) (b,2)
						);
					|_->
						MyGraphs.setState ng idv ROUGE
				);
		);
		(match MyGraphs.getNeighbour v 2 with
			|None->()(*Impossible ?*)
			|Some(a,p)->
				MyGraphs.addVertex ng (MyGraphs.createVertex a);
				MyGraphs.addEdge ng (idv,2) (a,3);
		);
		(*
		let Some(cPrev,p1) = MyGraphs.getNeighbour v 3 in
		let Some(cNext,p2) = MyGraphs.getNeighbour v 2 in
		(match MyGraphs.getState g cPrev with
		|CYAN ->
			MyGraphs.setState ng idv NOIR
		|_->
			(*!!!! Si le suivant est cyan, il a pu réagir, il ne faut donc pas s'y atacher s'il a réagit sinon BM de deg 3*)
			MyGraphs.setState ng idv ROUGE;
			MyGraphs.addVertex ng (MyGraphs.createVertex cPrev);
			MyGraphs.addEdge ng (idv,3) (cPrev,2);
			(match MyGraphs.getState g cNext with
				|CYAN->()
				| _ ->	
				MyGraphs.addVertex ng (MyGraphs.createVertex cNext);
				MyGraphs.addEdge ng (idv,2) (cNext,3);
			);
		);*)
	| CYAN ->(*<-P SCATTER*)
		(*Si je suis cyan et que mon voisin 2 (cercle) est orange, je deviens Noir*)
		(*Si je suis cyan et que mon voisin 2 n'est pas orange, je deviens Bleu*)
		(match MyGraphs.getNeighbour v 2 with
			|None -> ()(*Impossible ?*)
			|Some(a,p)->
				(match MyGraphs.getState g a with
					| ORANGE ->
						MyGraphs.setState ng idv NOIR;
					| _ ->
						MyGraphs.setState ng idv BLEU;
						MyGraphs.addVertex ng (MyGraphs.createVertex a);
						MyGraphs.addEdge ng (idv,2) (a,3);
						(match MyGraphs.getNeighbour v 3 with
							|None -> ()(*Impossible ?*)
							|Some(b,pp) ->
								MyGraphs.addVertex ng (MyGraphs.createVertex b);
								MyGraphs.addEdge ng (idv,3) (b,2)
						);

				);
		);
		


		(*
		let Some(cPrev,p1) = MyGraphs.getNeighbour v 3 in
		let Some(cNext,p2) = MyGraphs.getNeighbour v 2 in
		(match MyGraphs.getState g cNext with
		|ORANGE ->
			MyGraphs.setState ng idv NOIR
		|_->
			MyGraphs.addVertex ng (MyGraphs.createVertex cPrev);
			MyGraphs.addEdge ng (idv,3) (cPrev,2);
			MyGraphs.addVertex ng (MyGraphs.createVertex cNext);
			MyGraphs.addEdge ng (idv,2) (cNext,3);
			MyGraphs.setState ng idv BLEU
		);
		*)
	| FUCHSIA ->(*<-PP-> SCATTER*) 
		let Some(cNext,p2) = MyGraphs.getNeighbour v 2 in
		MyGraphs.setState ng idv ROUGE;
		MyGraphs.addVertex ng (MyGraphs.createVertex cNext);
		MyGraphs.addEdge ng (idv,2) (cNext,3)
	|_ ->()(*Impossible*)
	);(*print_string " OK\n";flush_all();*)
	(**retour->*)ng;;