let failwithf fmt = Printf.ksprintf failwith fmt

type name = string

type body = string list

type cclass = {
  cl_name    : name;
  cl_package : package;
  cl_extend  : cclass option;
  mutable cl_sealed : bool;
  mutable cl_fields : fields;
}

and ctype =
  | Bool
  | Int
  | Float
  | String
  | Cobject of cclass
  | Custom  of string
  | Enum    of enum

and arg = string * ctype

and field =
  | Method      of name * arg list * ctype option * (cclass -> body) option * bool
  | Event       of name * arg list
  | Override    of name * (cclass -> body) option
  | Variable    of name * ctype
  | Constructor of name * arg list * (cclass -> body) option
  | Port        of name * port
  | Collection  of name * port
  | Slot        of name * port

and fields = field list

and enum = {
  en_name    : string;
  en_package : package;
  en_members : string list;
}

and func = {
  fn_name : string;
  fn_args : arg list;
  fn_ret  : ctype option;
  fn_body : body option;
}

and package = {
  pk_name: string;
  mutable pk_classes : cclass list;
  mutable pk_enums   : enum list;
  mutable pk_funcs   : func list;
  mutable pk_declare : string list;
  mutable pk_sealed  : bool;
}

and port = {
  pt_target : cclass;
  pt_name   : string;
  pt_source : cclass;
}

let field_name = function
  | Method      (name, _, _, _, _)
  | Event       (name, _)
  | Override    (name, _)
  | Variable    (name, _)
  | Constructor (name, _, _)
  | Port        (name, _)
  | Collection  (name, _)
  | Slot        (name, _) -> name

let field_kind = function
  | Method      _ -> "method"
  | Event       _ -> "event"
  | Override    _ -> "override"
  | Variable    _ -> "variable"
  | Constructor _ -> "constructor"
  | Port        _ -> "port"
  | Collection  _ -> "collection"
  | Slot        _ -> "slot"

let int       = Int
let bool      = Bool
let float     = Float
let string    = String
let cobject t = Cobject t
let custom s  = Custom s

let arg name ctype = (name, ctype)

let group xs =
  List.fold_left (fun acc fields -> List.rev_append fields acc) [] xs

let meth name ?(static=false) ?ret ?body args =
  [Method (name, args, ret, body, static)]

let event name args =
  [Event (name, args)]

let override ?body name =
  [Override (name, body)]

let variable name ctype =
  [Variable (name, ctype)]

let constructor name ?body args =
  [Constructor (name, args, body)]

let collection name port =
  [Collection (name, port)]

let slot name port =
  [Slot (name, port)]

let fail_sealed sealed_kind sealed_name item_kind item_name =
  failwithf "Trying to insert %s %s in sealed %s %s"
    item_kind item_name sealed_kind sealed_name

let assert_unsealed_package pkg kind name =
  if pkg.pk_sealed then fail_sealed "package" pkg.pk_name kind name

let assert_unsealed_class cl kind name =
  if cl.cl_sealed then fail_sealed "class" cl.cl_name kind name

let package pk_name = {
  pk_name;
  pk_enums   = [];
  pk_classes = [];
  pk_funcs   = [];
  pk_declare = [];
  pk_sealed  = false;
}

let enum en_package en_name en_members =
  assert_unsealed_package en_package "enum" en_name;
  let enum = { en_name; en_package; en_members } in
  en_package.pk_enums <- enum :: en_package.pk_enums;
  Enum enum

let func fn_package fn_name ?ret ?body fn_args =
  assert_unsealed_package fn_package "func" fn_name;
  let func = { fn_name; fn_ret = ret; fn_body = body; fn_args } in
  fn_package.pk_funcs <- func :: fn_package.pk_funcs

let register cls =
  assert_unsealed_package cls.cl_package "class" cls.cl_name;
  cls.cl_package.pk_classes <- cls :: cls.cl_package.pk_classes;
  cls

let cclass cl_package cl_extend cl_name =
  register { cl_name; cl_extend; cl_fields = []; cl_package; cl_sealed = false }

let fields cl fields = match group fields with
  | [] -> ()
  | (x :: _) as xs ->
    assert_unsealed_class cl (field_kind x) (field_name x);
    cl.cl_fields <- List.rev_append xs cl.cl_fields

let seal_package pk =
  if not pk.pk_sealed then (
    pk.pk_sealed  <- true;
    pk.pk_classes <- List.rev pk.pk_classes;
    pk.pk_enums   <- List.rev pk.pk_enums;
    pk.pk_funcs   <- List.rev pk.pk_funcs;
    pk.pk_declare <- List.rev pk.pk_declare;
  )

let seal_class cl =
  if not cl.cl_sealed then (
    cl.cl_sealed <- true;
    cl.cl_fields <- List.rev cl.cl_fields;
  )

let port pt_target pt_name pt_source =
  let pt = { pt_target; pt_name; pt_source } in
  fields pt_target [[Port (pt_name, pt)]];
  pt

let declare pkg body =
  let declaration = match body with
    | [] -> "\"\""
    | x :: _ -> "\"" ^ String.escaped x ^ "\""
  in
  assert_unsealed_package pkg "declaration" declaration;
  pkg.pk_declare <- List.rev_append body pkg.pk_declare

module Introspect = struct
  type nonrec func = func = {
    fn_name : string;
    fn_args : arg list;
    fn_ret  : ctype option;
    fn_body : body option;
  }
  let package_name pk = pk.pk_name
  let package_classes pk = seal_package pk; pk.pk_classes
  let package_enums pk = seal_package pk; pk.pk_enums
  let package_funcs pk = seal_package pk; pk.pk_funcs
  let package_declarations pk = seal_package pk; pk.pk_declare
  let mangle pk name = pk.pk_name ^ "_" ^ name
  let class_package cl = cl.cl_package
  let class_fields cl = seal_class cl; cl.cl_fields
  let class_extend cl = cl.cl_extend
  let rec class_depth = function
    | { cl_extend = None } -> 0
    | { cl_extend = Some c } -> 1 + class_depth c
  let class_name cl = cl.cl_name
  let enum_package en = en.en_package
  let enum_name en = en.en_name
  let enum_members en = en.en_members
  let port_source pt = pt.pt_source
  let port_name pt = pt.pt_name
  let port_target pt = pt.pt_target
end

let goo = package "goo"

let goo_object = cclass goo None "object"

let () = fields goo_object [
    meth "destroy" [];
  ]

let () = seal_package goo

let cclass package ?(extend=goo_object) name xs =
  let cl = cclass package (Some extend) name in
  fields cl xs;
  cl
