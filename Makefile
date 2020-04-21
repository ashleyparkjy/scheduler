MODULES = authors main userSurvey courseJson classes
OBJECTS=$(MODULES:=.cmo)
TEST=test.byte
MAIN=main.byte
JSON=.json
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop
	
build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

clean:
	ocamlbuild -clean

launch:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

