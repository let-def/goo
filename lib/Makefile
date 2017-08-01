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
		$(foreach obj,$(TARGETS),$(obj).a) \
		$(wildcard *.cmi *.cmx *.h *.c)

uninstall:
	ocamlfind remove $(PKG)

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install

goo_lib.cma:
	$(OCAMLC) -c goo_ref.mli goo_ref.ml goo.mli goo.ml
	$(OCAMLC) -o goo_lib.cma -a goo_ref.cmo goo.cmo

goo_lib.cmxa:
	$(OCAMLOPT) -c goo_ref.mli goo_ref.ml goo.mli goo.ml
	$(OCAMLOPT) -o goo_lib.cmxa -a goo_ref.cmx goo.cmx
