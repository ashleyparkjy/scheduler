MODULES = authors main userSurvey command courseJson classes schedule algorithm visualize
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
SCHEDULETEST=schedule_test.byte
ALGORITHMTEST=algorithm_test.byte
VISUALTEST=visualize_test.byte
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
	$(OCAMLBUILD) -tag 'debug' $(VISUALTEST) && ./$(VISUALTEST)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private scheduler.zip

launch:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	ocamlbuild -clean
	zip scheduler.zip *

docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,lwt,cohttp,cohttp-lwt-unix \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,lwt,cohttp,cohttp-lwt-unix \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)