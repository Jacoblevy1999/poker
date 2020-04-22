default:
	utop -init poker.ml
	
build:
	ocamlbuild -use-ocamlfind poker.cmo test.cmo

test:
	ocamlbuild -use-ocamlfind -tag 'debug' test.byte && ./test.byte

check:
	bash checkenv.sh && bash checktypes.sh

finalcheck: check
	bash finalcheck.sh

docs:
	mkdir -p doc
	ocamldoc -d doc -html poker.ml

clean:
	ocamlbuild -clean
	rm -rf doc
