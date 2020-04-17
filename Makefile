MODULES = authors main userSurvey courseJson
OBJECTS=$(MODULES:=.cmo)
TEST=test.byte
MAIN=main.byte
JSON=.json
OCAMLBUILD=ocamlbuild -use-ocamlfind

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

clean:
	ocamlbuild -clean
	rm -f *$(JSON)

launch:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

