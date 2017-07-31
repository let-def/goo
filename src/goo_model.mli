(* A identifier name, must be a valid OCaml and C identifier *)
type name = string
(* A list of C statements *)
type body = string list

(* Packages are the root of declarations.
   A package contains a list of classes, enums and functions. *)
type package
val package : name -> package

type enum and cclass and port

(* Add custom code that will be inserted in the package *.h header. *)
val declare : package -> body -> unit

(* Goo has a simple type system.
   Basic types represent simple values, the ones that don't appear in the heap
   graph.
   *)
type ctype =
  | Bool
  | Int
  | Float
  | String
  | Cobject of cclass
  | Custom of string
  | Enum of enum
val int: ctype
val bool: ctype
val float: ctype
val string: ctype

(* Custom types maps to about arbitrary C types.
   The string will be output in the C generated code, but methods or functions
   with custom types won't be reflected OCaml. *)
val custom : string -> ctype

(* A C-like enumeration: a bunch of names that can be distinguished *)
val enum : package -> name -> string list -> ctype

(* Arguments of functions or methods are made from a name and type. *)
type arg = name * ctype
val arg: name -> ctype -> arg

(* Add a top-level function to a package.
   If no return type is provided, it will return "void".
   If body is specified, it will be used in C code. Otherwise, a stub is
   generated and will need to be implemented. *)
val func: package -> name -> ?ret:ctype -> ?body:body -> arg list -> unit

(* Classes are the main components of packages.
   They pack data and code and form the nodes of the heap graph.
   A class contains fields.
*)
type field =
  | Method      of name * arg list * ctype option * (cclass -> body) option * bool
  | Event       of name * arg list
  | Override    of name * (cclass -> body) option
  | Variable    of name * ctype
  | Constructor of name * arg list * (cclass -> body) option
  | Port        of name * port
  | Collection  of name * port
  | Slot        of name * port

type fields = field list

val cclass : package -> ?extend:cclass -> name -> fields list -> cclass
val cobject: cclass -> ctype
val group: fields list -> fields
val fields: cclass -> fields list -> unit

(* A constructor is a function that can allocate and return an instance of the
   class. *)
val constructor: name -> ?body:(cclass -> body) -> arg list -> fields

(* A variable will add a field to the "struct" that defines the data of the
   class.
   If the type is a `cobject`, the field will be generated "const" and a
   "set_name" method will be generated.
   This is done to keep track of the shape of the graph.
*)
val variable: name -> ctype -> fields

(* Methods are special functions:
   - their first argument is an instance of an object,
   - when inherited methods can be overriden,
   - dynamic methods support dynamic dispatch (calling the most precise
     implementation of the actual class, not the one known by the type of the
     variable).
*)
val meth: name -> ?static:bool -> ?ret:ctype -> ?body:(cclass -> body) -> arg list -> fields

(* Redefine a method. Arguments are taken the original definition. *)
val override: ?body:(cclass -> body) -> name -> fields

(* An event is a callback that can be implemented in OCaml side. *)
val event: name -> arg list -> fields

val port: cclass -> name -> cclass -> port
val collection : name -> port -> fields
val slot: name -> port -> fields


module Introspect : sig
  type func = {
    fn_name : string;
    fn_args : arg list;
    fn_ret  : ctype option;
    fn_body : body option;
  }
  val mangle : package -> name -> name
  val package_name : package -> name
  val package_classes : package -> cclass list
  val package_enums : package -> enum list
  val package_funcs : package -> func list
  val package_declarations : package -> body
  (*val package_*)
  val class_package : cclass -> package
  val class_fields : cclass -> fields
  val class_extend : cclass -> cclass option
  val class_name : cclass -> name
  val class_depth : cclass -> int
  val enum_package : enum -> package
  val enum_name : enum -> name
  val enum_members : enum -> string list
  val port_source : port -> cclass
  val port_name : port -> name
  val port_target : port -> cclass
end

(* Runtime support package *)
val goo : package
val goo_object : cclass
