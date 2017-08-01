GOO_LIB=$(shell ocamlfind query goo-lib)
CFLAGS = -g -I $(GOO_LIB)
LDFLAGS = -g -lui

GOO_SOURCES=$(GOO_LIB)/goo_system.c $(GOO_LIB)/goo_types.c $(GOO_LIB)/ml_goo.c
C_SOURCES = $(wildcard *.c) libui_stubs.c
C_OBJECTS = $(patsubst %.c, %.o, $(C_SOURCES)) $(patsubst $(GOO_LIB)/%.c, %.o, $(GOO_SOURCES))

ML_SOURCES = libui.ml controlgallery.ml

all: test

test: $(C_OBJECTS) $(ML_SOURCES)
	ocamlfind opt -package goo-lib -linkpkg -cclib "$(LDFLAGS)" -o $@ -g  -I . $^

$(ML_SOURCES) $(C_SOURCES): libui.ml

%.o: %.c %.h
	ocamlc -c -ccopt "$(CFLAGS)" -c -g $<

%.o: %.c
	ocamlc -c -ccopt "$(CFLAGS)" -c -g $<

%.o: $(GOO_LIB)/%.c
	ocamlc -c -ccopt "$(CFLAGS)" -c -g $<

clean:
	rm -f *.o

libui.ml: desc.ml
	ocaml desc.ml

libui_stubs.c: libui.ml
