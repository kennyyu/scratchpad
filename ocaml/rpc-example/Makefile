TARGETS = server.native client.native

all:
	eval `opam config -env` && \
	ocamlbuild -use-ocamlfind $(TARGETS) -cflags "-warn-error @A"

clean:
	eval `opam config -env` && ocamlbuild -clean