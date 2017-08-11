(* An identifier name, must be a valid OCaml and C identifier *)
type name = string

let failwithf fmt = Printf.ksprintf failwith fmt

module Sealed_list : sig
  type seal
  val seal: unit -> seal

  type 'a t
  val make : seal -> 'a t
  val append : 'a t -> 'a -> ('fmt, unit, string, unit) format4 -> 'fmt
  val read : 'a t -> 'a list
  val unsafe_rev_read : 'a t -> 'a list
end = struct
  type seal = bool ref
  let seal () = ref false
  type 'a t = { mutable items: 'a list; mutable sealed: bool; seal : seal }
  let make seal =
    if !seal then invalid_arg "Sealed_list.make: seal is already broken";
    { items = []; sealed = false; seal }

  let append xs x fmt =
    if !(xs.seal) then failwithf fmt
    else (
      xs.items <- x :: xs.items;
      Printf.ifprintf () fmt
    )

  let read xs =
    xs.seal := true;
    if not xs.sealed then (
      xs.sealed <- true;
      xs.items <- List.rev xs.items;
    );
    xs.items

  let unsafe_rev_read xs = xs.items
end

type 'a id = 'a Goo_id.t
let ignore_id : _ id -> unit = ignore

let inj = Goo_id.inj and prj = Goo_id.prj and name_of = Goo_id.name

type 'a sealed_list = 'a Sealed_list.t
let seal = Sealed_list.seal
let sealed_list = Sealed_list.make
let append = Sealed_list.append

(* Packages are the root of declarations.
   A package contains a list of classes, enums and functions. *)
type package_desc = {
  pk_enums: enum sealed_list;
  pk_classes: classe sealed_list;
  pk_funcs: func sealed_list;
}

and classe_desc = {
  cl_package: package;
  cl_extend: classe option;
  cl_funcs: func sealed_list;
  cl_events : event sealed_list;
  cl_relations : classe_relation sealed_list;
}

and classe_relation =
  | Rel_port of port
  | Rel_slot of slot
  | Rel_collection of collection

and func_desc = {
  fn_ret  : ctype list;
  fn_args : arg list;
  fn_kind : func_kind;
}

and func_kind =
  | Fn_class of classe
  | Fn_package of package

and event_desc = {
  ev_classe : classe;
  ev_args : arg list;
  ev_ret  : ctype list;
}

and enum_desc = {
  en_package: package;
  en_members: enum_member sealed_list;
}

and enum_member_desc = {
  enm_enum : enum;
}

and port_desc = {
  pt_source : classe;
  pt_target : classe;
}

and collection_desc = {
  col_classe : classe;
  col_port : port;
}

and slot_desc = {
  sl_classe : classe;
  sl_port : port;
}

and enum = enum_desc id
and enum_member = enum_member_desc id
and event = event_desc id
and func = func_desc id
and classe = classe_desc id
and package = package_desc id
and port = port_desc id
and collection = collection_desc id
and slot = slot_desc id

and arg = name * ctype

and ctype =
  | Bool
  | Int
  | Float
  | String
  | Object of classe
  | Object_option of classe
  | Custom of string
  | Flag of enum

let package name =
  let seal = Sealed_list.seal () in
  inj name {
    pk_enums = sealed_list seal;
    pk_classes = sealed_list seal;
    pk_funcs = sealed_list seal;
  }

let classe cl_package cl_extend name =
  let seal = seal () in
  let result = inj name {
      cl_package;
      cl_extend;
      cl_funcs = sealed_list seal;
      cl_relations = sealed_list seal;
      cl_events = sealed_list seal;
    } in
  append (prj cl_package).pk_classes result
    "adding class %s to package %s which is already sealed"
    name (name_of cl_package);
  result

let enum en_package name =
  let seal = seal () in
  let result = inj name { en_package; en_members = sealed_list seal; } in
  append (prj en_package).pk_enums result
    "adding enum %s to package %s which is already sealed"
    name (name_of en_package);
  result

let enum_member' enm_enum name =
  let result = inj name { enm_enum } in
  append (prj enm_enum).en_members result
    "adding member %s to enumeration %s which is already sealed"
    name (name_of enm_enum);
  result

let event' ev_classe ev_ret name ev_args =
  let ev = inj name { ev_classe; ev_ret; ev_args; } in
  append (prj ev_classe).cl_events ev
    "adding event %s to class %s which is already sealed"
    name (name_of ev_classe);
  ev

let enum_member enm_enum name =
  ignore_id (enum_member' enm_enum name)

let event ev_classe ev_ret name ev_args =
  ignore_id (event' ev_classe ev_ret name ev_args)

let int    = Int
let bool   = Bool
let float  = Float
let string = String
let flag enum = Flag enum
let arg name typ = (name, typ)
let objet cl = Object cl
let objet_opt cl = Object_option cl

let raw_func fn_kind fn_ret name fn_args =
  inj name { fn_args; fn_ret; fn_kind }

let func' pkg ret name args =
  let func = raw_func (Fn_package pkg) ret name args in
  append (prj pkg).pk_funcs func
    "adding function %s to package %s which is already sealed"
    name (name_of pkg);
  func

let meth' cl ret name args =
  let func = raw_func (Fn_class cl) ret name args in
  append (prj cl).cl_funcs func
    "adding method %s to class %s which is already sealed"
    name (name_of cl);
  func

let compare_classe cl1 cl2 =
  let rec is_ancestor ancestor cl =
    match (prj cl).cl_extend with
    | None -> false
    | Some cl' -> (cl' = ancestor || is_ancestor ancestor cl')
  in
  if cl1 = cl2 then `Eq else
  if is_ancestor cl1 cl2 then `Lt
  else if is_ancestor cl2 cl1 then `Gt
  else `Neq

let func_args_at_class func classe =
  match (prj func).fn_args with
  | ("self", Object cl) :: rest when compare_classe cl classe = `Lt  ->
    ("self", Object classe) :: rest
  | args -> args

let port pt_target name pt_source =
  let port = inj name { pt_target; pt_source } in
  append (prj pt_target).cl_relations (Rel_port port)
    "adding port %s to class %s which is already sealed"
    name (name_of pt_target);
  port

let slot' sl_classe name sl_port =
  let slot = inj name { sl_classe; sl_port } in
  append (prj sl_classe).cl_relations (Rel_slot slot)
    "adding slot %s to class %s which is already sealed"
    name (name_of sl_classe);
  slot

let collection' col_classe name col_port =
  let collection = inj name { col_classe; col_port } in
  append (prj col_classe).cl_relations (Rel_collection collection)
    "adding collection %s to class %s which is already sealed"
    name (name_of col_classe);
  collection

let func pkg ret name args =
  ignore_id (func' pkg ret name args)

let meth cl ret name args =
  ignore_id (meth' cl ret name args)

let slot sl_classe name sl_port =
  ignore_id (slot' sl_classe name sl_port)

let collection col_classe name col_port =
  ignore_id (collection' col_classe name col_port)

let seal_package pkg =
  ignore (Sealed_list.read (prj pkg).pk_classes)

let seal_classe cl =
  ignore (Sealed_list.read (prj cl).cl_funcs)

(* Runtime support package and root of object hierarchy *)
let goo = package "goo"

let goo_object = classe goo None "object"
let goo_destroy = meth' goo_object [] "destroy" ["self", Object goo_object]

let () = (
  seal_package goo;
  seal_classe goo_object
)

let classe package ?(extend=goo_object) name =
  classe package (Some extend) name

module Introspect = struct
  module Table = struct
    type ('a, 'b) table = ('a, 'b) Hashtbl.t
    let create () = Hashtbl.create 7
    let add  tbl k v = Hashtbl.add tbl k v
    let rem  tbl k   = Hashtbl.remove tbl k
    let mem  tbl k   = Hashtbl.mem tbl k
    let find tbl k   = Hashtbl.find tbl k
  end

  let name_of = name_of

  let read = Sealed_list.read

  let package_classes pkg = read (prj pkg).pk_classes
  let package_enums pkg = read (prj pkg).pk_enums
  let package_funcs pkg = read (prj pkg).pk_funcs

  let class_package cl = (prj cl).cl_package
  let class_extend cl = (prj cl).cl_extend
  let class_depth cl =
    let rec aux n cl = match class_extend cl with
      | None -> n
      | Some cl' -> aux (n + 1) cl'
    in
    aux 0 cl
  let class_funcs cl = read (prj cl).cl_funcs
  let class_events cl = read (prj cl).cl_events

  type class_relation = classe_relation =
    | Rel_port of port
    | Rel_slot of slot
    | Rel_collection of collection

  let class_relations cl = read (prj cl).cl_relations

  let enum_package en = (prj en).en_package
  let enum_members en = read (prj en).en_members
  let enum_member_enum enm = (prj enm).enm_enum

  type nonrec func_kind = func_kind =
  | Fn_class of classe
  | Fn_package of package

  let func_kind fn = (prj fn).fn_kind
  let func_ret fn = (prj fn).fn_ret
  let func_args ?at_class fn =
    match at_class with
    | None -> (prj fn).fn_args
    | Some cl -> func_args_at_class fn cl

  let port_source pt = (prj pt).pt_source
  let port_target pt = (prj pt).pt_target

  let slot_classe sl = (prj sl).sl_classe
  let slot_port sl = (prj sl).sl_port

  let collection_classe col = (prj col).col_classe
  let collection_port col = (prj col).col_port

  let event_classe ev = (prj ev).ev_classe
  let event_args ev = (prj ev).ev_args
  let event_ret ev = (prj ev).ev_ret
end
