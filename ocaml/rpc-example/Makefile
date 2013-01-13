EXECS = server.native client.native

all:
	eval `opam config -env` && \
	ocamlbuild -use-ocamlfind $(EXECS) -cflags "-w @A-4-33-23"

clean:
	ocamlbuild -clean