all: config counter people registry run simulation statistics utilities visualization

config: config.ml
	ocamlbuild -use-ocamlfind config.byte

counter: counter.ml
	ocamlbuild -use-ocamlfind counter.byte

people: people.ml
	ocamlbuild -use-ocamlfind people.byte

registry: registry.ml
	ocamlbuild -use-ocamlfind registry.byte

run: run.ml
	ocamlbuild -use-ocamlfind run.byte

simulation: simulation.ml
	ocamlbuild -use-ocamlfind simulation.byte

statistics: statistics.ml
	ocamlbuild -use-ocamlfind statistics.byte

utilities: utilities.ml
	ocamlbuild -use-ocamlfind utilities.byte

visualization: visualization.ml
	ocamlbuild -use-ocamlfind visualization.byte

clean:
	rm -rf _build *.byte
