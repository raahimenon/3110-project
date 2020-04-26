MODULES= gameVars window room gamestate entity player animations buff combat main vector
OBJECTS=$(MODULES:=.cmo)
MLIS=$(MODULES:=mli)
MLS=$(MODULES:=ml)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind 

default: build
	utop

build:
	$(OCAMLBUILD) -tag 'debug' $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) -tag 'debug' $(MAIN) && ./$(MAIN)

clean:
	ocamlbuild -clean


