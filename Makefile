MODULES= authors gameVars window room gamestate entity player animations buff combat main vector load save room_gen
OBJECTS=$(MODULES:=.cmo)
MLIS=$(MODULES:=.mli)
MLS=$(MODULES:=.ml)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) -tag 'debug' $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

docs: build
	mkdir -p docs
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,sdl2 \
		-html -stars -d docs $(MLIS)

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && ./$(MAIN)

zip: clean
	zip -r game.zip *.ml* *.json _tags Makefile .ocamlinit .merlin sprites saves *.md INSTALLATION.md

clean:
	ocamlbuild -clean
	rm -rf game.zip *.byte