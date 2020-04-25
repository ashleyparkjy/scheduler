MODULES = authors main userSurvey command courseJson classes schedule algorithm
OBJECTS=$(MODULES:=.cmo)
TEST=test.byte
SCHEDULETEST=schedule_test.byte
ALGORITHMTEST=algorithm_test.byte
MAIN=main.byte
JSON=.json
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop
	
build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)
	$(OCAMLBUILD) -tag 'debug' $(SCHEDULETEST) && ./$(SCHEDULETEST)
	$(OCAMLBUILD) -tag 'debug' $(ALGORITHMTEST) && ./$(ALGORITHMTEST)

clean:
	ocamlbuild -clean

launch:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

