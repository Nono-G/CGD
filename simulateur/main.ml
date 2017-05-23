open Rule
(*
open RuleMovingHead
open RuleTriangles
*)

module CGD = RuleMovingHead

let _=
	(* *** PARAMS *** *)
	let init_size = (10) in (*Ici le n-uplet de paramètres pour la fonction init_graph*)
	let nb_steps = 100 in (*Ici le nombre d'étapes a effectuer*)
	(* *** PARAMS *** *)
	let init_graph=CGD.init_graph init_size in
		print_string("Initial graph generated\n");
		flush_all();
		Rule.MyGraphs.export init_graph "graphs/graphe_step0.json"; (* j'exporte le graphe initial dans le fichier graphe_initial.json *)	
		print_string("Initial graph xported\n");
		flush_all();
		
		for i=0 to nb_steps do
			print_string("Step "^(string_of_int i)^"\n");
			flush_all();
			(*let image_graph = Rule.run_rule Rule.my_rule init_graph in*)
			let image_graph = Rule.run_rule CGD.cgd_rule init_graph in
			Rule.copy init_graph image_graph;
			Rule.MyGraphs.export init_graph ("graphs/graphe_step"^(string_of_int (i+1))^".json");
		done