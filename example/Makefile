# Building main: we need to link together the Haskell main with the
# ocaml library (std library extended with our compiled module)
Main: Main.hs ocamlmod.a
	ghc Main.hs ocamlmod.a -lcurses

# Building the ocaml library: copying the standard library, making it
# modifiable and adding the object file test.o
ocamlmod.a: test.o
	cp /usr/local/lib/ocaml/libasmrun.a ocamlmod.a
	chmod 644 ocamlmod.a
	ar r ocamlmod.a test.o

# Standard compiling of an ocaml module into an object file.
test.o: test.ml
	ocamlopt -output-obj -o test.o test.ml

clean:
	rm *{~,.o,.a,.hi,.cmi,.cmx}
