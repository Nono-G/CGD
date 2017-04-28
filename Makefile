
main: main.ml rule.cmo graph.cmo ruleMovingHead.cmo ruleTriangles.cmo ruleBallHitting.cmo ruleMeyer.cmo ruleMeyer2.cmo ruleMult.cmo ruleSorbetiere.cmo ruleAlumette.cmo
	ocamlc graph.cmo rule.cmo ruleMovingHead.cmo ruleTriangles.cmo ruleBallHitting.cmo ruleMeyer.cmo ruleMeyer2.cmo ruleMult.cmo ruleSorbetiere.cmo ruleAlumette.cmo main.ml -o main

ruleTriangles.cmo : graph.cmo rule.cmo ruleTriangles.ml
	ocamlc graph.cmo rule.cmo ruleTriangles.ml

ruleMovingHead.cmo : graph.cmo rule.cmo ruleMovingHead.ml
	ocamlc graph.cmo rule.cmo ruleMovingHead.ml

ruleBallHitting.cmo : graph.cmo rule.cmo ruleBallHitting.ml
	ocamlc graph.cmo rule.cmo ruleBallHitting.ml

ruleMeyer.cmo : graph.cmo rule.cmo ruleMeyer.ml
	ocamlc graph.cmo rule.cmo ruleMeyer.ml

ruleMeyer2.cmo : graph.cmo rule.cmo ruleMeyer2.ml
	ocamlc graph.cmo rule.cmo ruleMeyer2.ml

ruleMult.cmo : graph.cmo rule.cmo ruleMult.ml
	ocamlc graph.cmo rule.cmo ruleMult.ml

ruleSorbetiere.cmo : graph.cmo rule.cmo ruleSorbetiere.ml
	ocamlc graph.cmo rule.cmo ruleSorbetiere.ml

ruleAlumette.cmo : graph.cmo rule.cmo ruleAlumette.ml
	ocamlc graph.cmo rule.cmo ruleAlumette.ml

rule.cmo:	graph.cmo rule.ml
	ocamlc graph.cmo rule.ml

cgd.cmo: graph.cmo cgd.ml
	ocamlc graph.cmo cgd.ml

graph.cmo: graph.ml
	ocamlc graph.ml
	
clean:
	rm *.cmo *.cmi main a.out Output/*
