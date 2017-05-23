main: compile cleanCompileDust

compile: main.ml rule.cmo graph.cmo ruleMovingHead.cmo ruleTriangles.cmo
	ocamlc graph.cmo rule.cmo ruleMovingHead.cmo ruleTriangles.cmo main.ml -o cgdsim

ruleTriangles.cmo : graph.cmo rule.cmo ruleTriangles.ml
	ocamlc graph.cmo rule.cmo ruleTriangles.ml

ruleMovingHead.cmo : graph.cmo rule.cmo ruleMovingHead.ml
	ocamlc graph.cmo rule.cmo ruleMovingHead.ml

rule.cmo: graph.cmo rule.ml
	ocamlc graph.cmo rule.ml

graph.cmo: graph.ml
	ocamlc -c graph.ml
	
cleanCompileDust:
	-rm *.cmo
	-rm *.cmi
	-rm a.out
