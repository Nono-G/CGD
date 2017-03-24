
main: main.ml rule.cmo graph.cmo
	ocamlc graph.cmo rule.cmo main.ml -o main


rule.cmo:	graph.cmo rule.ml
	ocamlc graph.cmo rule.ml

cgd.cmo: graph.cmo cgd.ml
	ocamlc graph.cmo cgd.ml


graph.cmo: graph.ml
	ocamlc graph.ml
	


clean:
	rm *.cmo *.cmi main a.out
