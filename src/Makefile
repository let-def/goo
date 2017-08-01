PKG=goo-gen
OCAMLC=ocamlc.opt
OCAMLOPT=ocamlopt.opt
OBJECTS=goo_gen.cma goo_gen.cmxa

all: $(OBJECTS)

clean:
	rm -f *.cm* *.o *.a

install: $(OBJECTS)
	ocamlfind install $(PKG) META $(OBJECTS) $(wildcard *.cmi)

uninstall:
	ocamlfind remove $(PKG)

reinstall:
	-$(MAKE) uninstall
	$(MAKE) install

goo_gen.cma:
	$(OCAMLC) -c goo_model.mli goo_model.ml goo_c.ml goo_ml.ml
	$(OCAMLC) -o goo_gen.cma -a goo_model.cmo goo_c.cmo goo_ml.cmo

goo_gen.cmxa:
	$(OCAMLOPT) -c goo_model.mli goo_model.ml goo_c.ml goo_ml.ml
	$(OCAMLOPT) -o goo_gen.cmxa -a goo_c.cmx goo_ml.cmx
