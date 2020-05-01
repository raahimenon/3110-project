MODULES= authors gameVars window room gamestate entity player animations buff combat main vector load save room_gen wfc_test
OBJECTS=$(MODULES:=.cmo)
MLIS=$(MODULES:=mli)
MLS=$(MODULES:=ml)
TEST=test.byte
MAIN=main.byte
WFC=wfc_test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) -tag 'debug' $(OBJECTS)

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && ./$(MAIN)

zip: clean
	zip -r game.zip *.ml* *.json _tags Makefile .ocamlinit .merlin sprites saves *.md INSTALLATION.md

clean:
	ocamlbuild -clean
	rm -rf game.zip *.byte

wfc:
	$(OCAMLBUILD) -tag 'debug' $(WFC) && ./$(WFC)


