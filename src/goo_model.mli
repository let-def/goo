(* An identifier name, must be a valid OCaml and C identifier *)
type name = string

module Id : sig
  type 'a t
  val inj : name -> 'a -> 'a t
  val prj : 'a t -> 'a
  val name : 'a t -> name
end
type 'a id = 'a Id.t
type void
val forget : _ id -> void id

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
   graph.
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

val int: ctype
val bool: ctype
val float: ctype
val string: ctype
val flag: enum -> ctype

(* Arguments of functions or methods are made from a name and type. *)
type arg = name * ctype
val arg: name -> ctype -> arg

(* Add a top-level function to a package.
   If no return type is provided, it will return "void".
   If body is specified, it will be used in C code. Otherwise, a stub is
   generated and will need to be implemented. *)
type func = func_desc id
type event = event_desc id
val func' : package -> ctype list -> name -> arg list -> func
val meth' : classe -> ctype list -> name -> arg list -> func
val event' : classe -> ctype list -> name -> arg list -> event
val func : package -> ctype list -> name -> arg list -> unit
val meth : classe -> ctype list -> name -> arg list -> unit
val event : classe -> ctype list -> name -> arg list -> unit

type port = port_desc id
type collection = collection_desc id
type slot = slot_desc id
val port        : classe -> name -> classe -> port
val collection' : classe -> name -> port -> collection
val slot'       : classe -> name -> port -> slot
val collection : classe -> name -> port -> unit
val slot       : classe -> name -> port -> unit

(* Runtime support package and root of object hierarchy *)
val goo : package
val goo_object : classe
val goo_destroy : func

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
  val event_args : event -> arg list
end
