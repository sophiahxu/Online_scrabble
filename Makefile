.PHONY: test

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

end:
	OCAMLRUNPARAM=b dune exec bin/end.exe

zip:
	rm -f scrabble.zip
	zip -r scrabble.zip . -x@exclude.lst

docs:
	dune build @doc

clean:
	dune clean
	rm -f scrabble.zip

loc:
	dune clean
	rm -f scrabble.zip
	cloc --by-file --include-lang=OCaml .