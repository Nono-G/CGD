open Rule










let _=
	let init_graph=Rule.MyGraphs.create () in
		let vertex=Rule.MyGraphs.createVertex "toto" in
		Rule.MyGraphs.addVertex init_graph vertex; (* Mon graphe initial: 1 sommet, nommé 'toto' *)
		Rule.MyGraphs.setState init_graph "toto" NOIR; (* Je lui donne un état noir *)
		print_string("Initial graph generated\n");
		flush_all();
		Rule.MyGraphs.export_gml init_graph "Output/graphe_initial.gml"; (* j'exporte le graphe initial dans le fichier graphe_initial.json *)	
		print_string("Initial graph xported\n");
		flush_all();
		
		for i=0 to 4 do (** /!\ 5 étapes c'est bien assez pour cette dynamique (croissance exponentielle du graphes /!\ **)
			print_string("Step "^(string_of_int i)^"\n");
		flush_all();
		
			let image_graph = Rule.run_rule Rule.my_rule init_graph in
			Rule.copy init_graph image_graph;
			Rule.MyGraphs.export_gml init_graph ("Output/graphe_step"^(string_of_int i)^".gml");
		done
