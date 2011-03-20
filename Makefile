
standard: clean
	ocamlopt -o server -thread -I +threads -I unix unix.cmxa threads.cmxa server.ml
	ocamlopt -o client unix.cmxa str.cmxa client.ml

clean: 
	rm -f *.cmi *.cmo *.o *.cmx 

sig: clean
	ocamlopt -i -thread -I +threads -I unix unix.cmxa threads.cmxa server.ml
	ocamlopt -i unix.cmxa str.cmxa client.ml || true
