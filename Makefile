test:
	ocamlfind ocamlc  -package yojson -package str -package ANSITerminal -linkpkg card.mli player.mli AI.mli save_game.mli game_engine.mli card.ml player.ml AI.ml save_game.ml whoknows.ml game_engine.ml -o game

play:
	ocamlfind ocamlc  -package yojson -package str -package ANSITerminal -linkpkg card.mli player.mli AI.mli save_game.mli game_engine.mli card.ml player.ml AI.ml save_game.ml whoknows.ml game_engine.ml -o game
	ocamlbuild main.byte && ./main.byte

check:
	bash checkenv.sh

clean:
	ocamlbuild -clean
