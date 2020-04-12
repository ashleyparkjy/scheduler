MODULES = authors main userSurvey courseJson
OBJECTS=$(MODULES:=.cmo)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

clean:
	ocamlbuild -clean

launch:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

