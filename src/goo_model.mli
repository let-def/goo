(* An identifier name, must be a valid OCaml and C identifier.
   An id is just a wrapper that gives a value a physical identity.

   The modoule gives a formal definition of all entities.
   See [examples/libui/desc.ml] for a sample use of the entities.
*)
type name = string
type 'a id = 'a Goo_id.t

(* Abstract types for all entities manipulated in the model. *)
type classe_desc
type collection_desc
type enum_desc
type enum_member_desc
type event_desc
type func_desc
type package_desc
type port_desc
type slot_desc

(* Packages are the root of declarations.
   A package contains a list of classes, enums and functions. *)
type package = package_desc id
val package : name -> package

type classe = classe_desc id
val classe : package -> ?extend:classe -> name -> classe

(* A C-like enumeration: a bunch of names that can be distinguished *)
type enum = enum_desc id
type enum_member = enum_member_desc id
val enum : package -> name -> enum
val enum_member' : enum -> name -> enum_member
val enum_member : enum -> name -> unit

(* Goo has a simple type system.
   Basic types represent simple values, the ones that don't appear in the heap
   graph.  `Object' and `Object_option' are the two kind of values that affect
   the heap graph.
   `Flag' is a value taken from an enumeration.
   `Custom' is an arbitrary string that is given to the backend and may or may
   not make sense. (TODO: that's where Ctypes should be integrated).
   *)
type ctype =
  | Bool
  | Int
  | Float
  | String
  | Object of classe
  | Object_option of classe
  | Custom of string
  | Flag of enum

(* Some sugar for basic types. *)
val int: ctype
val bool: ctype
val float: ctype
val string: ctype
val flag: enum -> ctype

(* Arguments of functions or methods are made from a name and type.
   Right now they are nothing more than a pair.  *)
type arg = name * ctype
val arg: name -> ctype -> arg

(* Functions for basic definitions come in two flavors:
   - the base one just register the definition
   - the one suffixed with `'` returns an opaque name that witnesses the
     definition.
   This name can be used to pass meta-data / specific knowledge to the backend.
   For instance, `Goo_c.set_dynamic a_method` informs the C backend that a
   method returned by `meth'` has dynamic dispatch.
*)
type func = func_desc id
type event = event_desc id

(* A function declaration is read in the "C" order:
     func <package> <return types> <function name> <parameters>;

     Multiple return types are allowed:
     - an empty list maps to "void"
     - a singleton maps to a normal C return-type
     - a tuple maps to a list of pointers that are expected to be filled by the
       C function.

    func math [float;float] "transpose" [arg "re" float; arg "im" float]

    maps to
        void math_transpose(double re, double im, double *ret0, double *ret1)
        val math_transpose : float -> float -> float * float
*)
val func : package -> ctype list -> name -> arg list -> unit
val func' : package -> ctype list -> name -> arg list -> func
(* Method are nothing more than functions that belong to a class rather than
   belonging to a package.
   As far as the interface is concerned, this is just an informal difference
   that is used by code generator to generate names and put the function
   definition close to the class definition.
   To make it closer to a "usual" method, the first argument should be an
   object of the same class the method belongs too (e.g "meth_class *self").
*)
val meth : classe -> ctype list -> name -> arg list -> unit
val meth' : classe -> ctype list -> name -> arg list -> func
(* Events.
   Events allow control to call back to the interface language.
   Each event is an optional closure that can be set from ML.
   Contrary to methods, events implicitly take an object of the class as first
   argument. Method can be "static" while events are always bound to an
   instance.
*)
val event' : classe -> ctype list -> name -> arg list -> event
val event : classe -> ctype list -> name -> arg list -> unit

(* Relations.
   The structure of object graph is made explicit by the use of relations.
   There are three concepts of relations: port, slots and collections.

   A port is the endpoint of a relation. It can be empty (mapped to NULL /
   None) or connected to a slot or a collection.
   The declaration below reads "a control can have a single `parent` which is
   itself a control."
   A slot can connect to zero or one port.
   A collection can connect to zero or many ports.

   For instance, a window has a slot which is the root widget. A list layout
   has a collection, the sequence of all widgets that are listed.

   Symmetry is enforced: if button is the children of window, then window
   will be the parent of button.
*)
type port = port_desc id
type collection = collection_desc id
type slot = slot_desc id
val port        : classe -> name -> classe -> port
val slot        : classe -> name -> port -> unit
val slot'       : classe -> name -> port -> slot
val collection  : classe -> name -> port -> unit
val collection' : classe -> name -> port -> collection

(* Runtime support package and root of object hierarchy *)
val goo : package
val goo_object : classe
val goo_destroy : func

(* That's all: the rest is functions used by backends to introspect
   definitions. *)
module Introspect : sig
  module Table : sig
    type ('a, 'b) table
    val create : unit -> ('a id, 'b) table
    val add  : ('a, 'b) table -> 'a -> 'b -> unit
    val rem  : ('a, 'b) table -> 'a -> unit
    val mem  : ('a, 'b) table -> 'a -> bool
    val find : ('a, 'b) table -> 'a -> 'b
  end

  val name_of : _ id -> name

  val package_classes : package -> classe list
  val package_enums   : package -> enum list
  val package_funcs   : package -> func list

  val class_package   : classe -> package
  val class_extend    : classe -> classe option
  val class_depth     : classe -> int
  val class_funcs     : classe -> func list
  val class_events    : classe -> event list

  type class_relation =
    | Rel_port of port
    | Rel_slot of slot
    | Rel_collection of collection

  val class_relations : classe -> class_relation list

  val enum_package : enum -> package
  val enum_members : enum -> enum_member list
  val enum_member_enum : enum_member -> enum

  type func_kind =
    | Fn_class of classe
    | Fn_package of package

  val func_kind : func -> func_kind
  val func_ret : func -> ctype list
  val func_args : ?at_class:classe -> func -> arg list

  val port_source : port -> classe
  val port_target : port -> classe

  val slot_classe : slot -> classe
  val slot_port   : slot -> port

  val collection_classe : collection -> classe
  val collection_port   : collection -> port

  val event_classe : event -> classe
  val event_ret : event -> ctype list
  val event_args : ?with_self:bool -> event -> arg list
end
