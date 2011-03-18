
standard: clean
	ocamlopt -o server unix.cmxa server.ml
	ocamlopt -o client unix.cmxa str.cmxa client.ml

clean: 
	rm -f *.cmi *.cmo *.o *.cmx 

sig: clean
	ocamlopt -i unix.cmxa server.ml || true
	ocamlopt -i unix.cmxa str.cmxa client.ml || true
