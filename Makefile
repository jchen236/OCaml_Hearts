test:
	ocamlfind ocamlc -package yojson -package str -package ANSITerminal -linkpkg card.mli player.mli AI.mli IOops.mli game_engine.mli card.ml player.ml AI.ml IOops.ml game_logic.ml game_engine.ml -o game
	mv *.cmi *.cmxa *.cma *.cmx *.cmo *.o *.so *.a _build
play:

	ocamlfind ocamlc -package yojson -package str -package ANSITerminal -linkpkg card.mli player.mli AI.mli IOops.mli game_engine.mli card.ml player.ml AI.ml IOops.ml game_logic.ml game_engine.ml main.ml -o game
	mv *.cmi *.cmxa *.cma *.cmx *.cmo *.o *.so *.a _build
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal main.byte && ./main.byte

check:
	bash checkenv.sh

clean:
	ocamlbuild -clean
	rm -f *.cmi *.cmxa *.cma *.cmx *.cmo *.o *.so *.a