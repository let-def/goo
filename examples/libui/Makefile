GOO_LIB=$(shell ocamlfind query goo-lib)
CFLAGS = -g -I $(GOO_LIB)
LDFLAGS = -g -lui

C_SOURCES = $(wildcard *.c) libui_stubs.c
C_OBJECTS = $(patsubst %.c, %.o, $(C_SOURCES))

ML_SOURCES = libui.ml controlgallery.ml

all: test

test: $(C_OBJECTS) $(ML_SOURCES)
	ocamlfind opt -package goo-lib -linkpkg -cclib "$(LDFLAGS)" -o $@ -g  -I . $^

controlgallery.ml $(C_SOURCES): libui.ml libui.h

%.o: %.c %.h
	ocamlc -c -ccopt "$(CFLAGS)" -c -g $<

%.o: %.c
	ocamlc -c -ccopt "$(CFLAGS)" -c -g $<

%.o: $(GOO_LIB)/%.c
	ocamlc -c -ccopt "$(CFLAGS)" -c -g $<

clean:
	rm -f *.o libui_stubs.c libui.h

.PHONY: libui.ml

libui.ml:
	ocaml desc.ml

libui_stubs.c libui.h: libui.ml 
