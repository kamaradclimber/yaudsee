
standard: clean
	ocamlopt -c -I unix unix.cmxa nums.cmxa  str.cmxa utils.ml
	ocamlopt -o server -thread -I +threads -I unix unix.cmxa nums.cmxa threads.cmxa str.cmxa utils.cmx server.ml
	ocamlopt -o client -thread -I +threads -I unix unix.cmxa nums.cmxa threads.cmxa str.cmxa utils.cmx client.ml

doc: clean
	mkdir -p doc
	ocamlopt -c -I unix unix.cmxa nums.cmxa  str.cmxa utils.ml
	ocamldoc -html -d doc  -I +threads -I unix  utils.ml server.ml client.ml
clean: 
	rm -f *.cmi *.cmo *.o *.cmx *.html

sig: clean
	ocamlopt -c -I unix unix.cmxa nums.cmxa  str.cmxa utils.ml
	ocamlopt -i -thread -I +threads -I unix unix.cmxa threads.cmxa nums.cmxa str.cmxa utils.cmx server.ml || true
	ocamlopt -i  -thread -I +threads -I unix unix.cmxa threads.cmxa nums.cmxa str.cmxa utils.cmx client.ml ||true
