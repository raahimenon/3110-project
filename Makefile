MODULES=loading
OBJECTS=$(MODULES:=.cmo)
MLIS=$(MODULES:=mli)
MLS=$(MODULES:=ml)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(LT) && ./$(LT)

clean:
	ocamlbuild -clean


