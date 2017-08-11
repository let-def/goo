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

SOURCES=goo_id.mli goo_id.ml goo_model.mli goo_model.ml goo_c.ml goo_ml.ml

goo_gen.cma: $(SOURCES)
	$(OCAMLC) -g -c $(SOURCES)
	$(OCAMLC) -o goo_gen.cma -a goo_id.cmo goo_model.cmo goo_c.cmo goo_ml.cmo

goo_gen.cmxa: $(SOURCES)
	$(OCAMLOPT) -g -c $(SOURCES)
	$(OCAMLOPT) -o goo_gen.cmxa -a goo_id.cmx goo_model.cmx goo_c.cmx goo_ml.cmx
