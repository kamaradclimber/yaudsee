
standard: clean
	ocamlopt -o server -thread -I +threads -I unix unix.cmxa nums.cmxa threads.cmxa server.ml
	ocamlopt -o client -thread -I +threads -I unix unix.cmxa nums.cmxa threads.cmxa str.cmxa client.ml

clean: 
	rm -f *.cmi *.cmo *.o *.cmx 

sig: clean
	ocamlopt -i -thread -I +threads -I unix unix.cmxa threads.cmxa nums.cmxa server.ml || true
	ocamlopt -i  -thread -I +threads -I unix unix.cmxa threads.cmxa nums.cmxa str.cmxa client.ml ||true
