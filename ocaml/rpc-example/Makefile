TARGETS = server.native client.native

all:
	eval `opam config -env` && \
	ocamlbuild -use-ocamlfind $(TARGETS) -cflags "-w @A-4-33-23"

clean:
	eval `opam config -env` && ocamlbuild -clean