PKG=goo-lib
OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
TARGETS=goo_lib
OBJECTS=$(foreach obj,$(TARGETS),$(obj).cma $(obj).cmxa)

all: $(OBJECTS)

clean:
	rm -f *.cm* *.o *.a

install: $(OBJECTS)
	ocamlfind install $(PKG) META $(OBJECTS) \
		$(foreach obj,$(TARGETS),$(obj).a) libgoo.a \
		$(wildcard *.cmi *.cmx *.h)

uninstall:
	ocamlfind remove $(PKG)

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install

SOURCES=goo_ref.mli goo_ref.ml goo.mli goo.ml

libgoo.a: goo_system.c ml_goo.c
	$(OCAMLC) -g -c goo_system.c ml_goo.c
	ar cr libgoo.a goo_system.o ml_goo.o 

goo_lib.cma: $(SOURCES) libgoo.a
	$(OCAMLC) -g -c $(SOURCES)
	$(OCAMLC) -o goo_lib.cma -a goo_ref.cmo goo.cmo -custom -cclib lgoo.a

goo_lib.cmxa: $(SOURCES) libgoo.a
	$(OCAMLOPT) -g -c $(SOURCES)
	$(OCAMLOPT) -o goo_lib.cmxa -a goo_ref.cmx goo.cmx -cclib -lgoo
