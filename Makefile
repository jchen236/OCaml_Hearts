test:
	ocamlbuild -pkg oUnit test.byte && ./test.byte

play:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal main.byte && ./main.byte

check:
	bash checkenv.sh

clean:
	ocamlbuild -clean