open Rule
(*
open RuleMovingHead
open RuleTriangles
open RuleBallHitting
open RuleMeyer
open RuleMeyer2
open RuleMult
open RuleSorbetiere
*)
module CGD = RuleSorbetiere


let _=
	let init_size = (6,3) in
	let nb_steps = 400 in
	let init_graph=CGD.init_graph init_size in
		print_string("Initial graph generated\n");
		flush_all();
		Rule.MyGraphs.export init_graph "Output/graphe_step0.json"; (* j'exporte le graphe initial dans le fichier graphe_initial.json *)	
		print_string("Initial graph xported\n");
		flush_all();
		
		for i=0 to nb_steps do (** /!\ 5 étapes c'est bien assez pour cette dynamique (croissance exponentielle du graphes /!\ **)
			print_string("Step "^(string_of_int i)^"\n");
			flush_all();
		
			(*let image_graph = Rule.run_rule Rule.my_rule init_graph in*)
			let image_graph = Rule.run_rule CGD.cgd_rule init_graph in
			Rule.copy init_graph image_graph;
			Rule.MyGraphs.export init_graph ("Output/graphe_step"^(string_of_int (i+1))^".json");
		done