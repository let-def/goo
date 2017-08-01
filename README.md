# Goo: Cross-runtime object interface generation

_Goo is experimental_.

Goo is a tool for writing high-level bindings. GUI toolkits are a natural use-case, but it is suitable for other libraries managing complex state and control flow, especially if they have an object-oriented interface.

[Ctypes](https://github.com/ocamllabs/ocaml-ctypes) is effective for low-level FFI and fine grained manipulation of memory. However tracking objects lifetimes and relations is hard.

Goo focus on this part of the problem: while losing some of the flexibility and efficiency of low-level bindings, it tracks and restrict the shape of the heap graph to offer cheap, type-safe and memory-safe bindings.

## Approach

The core of Goo is an object model that is well suited for describing the structure of object graphs (e.g. relations of widgets in a window).  It has a simple feature set that maps well to mainstream object languages.

A binding starts with a description (see [libui](examples/libui/desc.ml) example) of an object graph. This graph is consumed by two code generators: one for the interface, one for the implementation. A C API is used as the "rendez-vous" point between both generators.

Right now the interface generator produces OCaml code and the implementation generator a subset of C that encodes some more guarantees than usual C code, but it still compatible with a normal C compiler.

# Examples

For quick'n'dirty results, a Goo [description](examples/libui/desc.ml) of [libui](https://github.com/andlabs/libui) was written to illustrate the workflow:

![Libui bindings running on Gtk](examples/libui/doc/gtk.png?raw=true "OCaml/Goo/Libui/Gtk")
![Libui bindings running on OS X](examples/libui/doc/osx.png?raw=true "OCaml/Goo/Libui/OSX")

# TODO

- region management
- multiple return values
- ctypes integration

# Future work

There is nothing OCaml specific in Goo semantics. Adding backends for other languages should be a reasonable amount of work.
