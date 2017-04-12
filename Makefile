
main: main.ml rule.cmo graph.cmo ruleMovingHead.cmo ruleTriangles.cmo ruleBallHitting.cmo
	ocamlc graph.cmo rule.cmo ruleMovingHead.cmo ruleTriangles.cmo ruleBallHitting.cmo main.ml -o main

ruleTriangles.cmo : graph.cmo rule.cmo ruleTriangles.ml
	ocamlc graph.cmo rule.cmo ruleTriangles.ml

ruleMovingHead.cmo : graph.cmo rule.cmo ruleMovingHead.ml
	ocamlc graph.cmo rule.cmo ruleMovingHead.ml

ruleBallHitting.cmo : graph.cmo rule.cmo ruleBallHitting.ml
	ocamlc graph.cmo rule.cmo ruleBallHitting.ml

rule.cmo:	graph.cmo rule.ml
	ocamlc graph.cmo rule.ml

cgd.cmo: graph.cmo cgd.ml
	ocamlc graph.cmo cgd.ml


graph.cmo: graph.ml
	ocamlc graph.ml
	


clean:
	rm *.cmo *.cmi main a.out Output/*
