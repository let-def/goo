open Goo_model
module I = Introspect

let class_name cl = I.(mangle (class_package cl) (class_name cl))
let enum_name en = I.(mangle (enum_package en) (enum_name en))
let method_name cl name = class_name cl ^ "_" ^ name

let sprint = Printf.sprintf
let print o fmt = Printf.ksprintf o fmt
let on_ x y = "on_" ^ x ^ "_" ^ y

let ctype ident = function
  | Bool      -> sprint "goo_bool %s" ident
  | Int       -> sprint "int %s" ident
  | Float     -> sprint "double %s" ident
  | String    -> sprint "goo_string %s" ident
  | Enum e    -> sprint "%s %s" (enum_name e) ident
  | Cobject t -> sprint "%s *%s" (class_name t) ident
  | Custom s  -> sprint "%s %s" s ident

let ctype_opt = function
  | None -> "void "
  | Some x -> ctype "" x

let rec iter_all_fields c f =
  begin match I.class_extend c with
    | None -> ()
    | Some c' -> iter_all_fields c' f
  end;
  List.iter (f c) (I.class_fields c)

let number_of_properties c =
  let count = ref 0 in
  iter_all_fields c (fun _ -> function
      | Variable (_, Cobject _) | Collection _ | Slot _ -> incr count
      | Port _ -> count := !count + 2
      | _ -> ()
    );
  !count

let property_index c name =
  let module M = struct exception Found of int end in
  let count = ref 0 in
  match iter_all_fields c (fun _ -> function
      | Variable (name', Cobject _) | Port (name', _)
      | Collection (name', _) | Slot (name', _)
        when name = name' -> raise (M.Found !count)
      | Variable (_, Cobject _) | Collection _ | Slot _ -> incr count
      | Port _ -> count := !count + 2
      | _ -> ()
    )
  with
  | () -> raise Not_found
  | exception (M.Found id) -> id

let param_list = function
  | [] -> "void"
  | params ->
    String.concat ", " (List.map (fun (name, ty) -> ctype name ty) params)

let add_self c params = ("self", Cobject c) :: params

let param_self_list c params = param_list (add_self c params)

let arg_list xs = String.concat ", " (List.map fst xs)

let print_proxy o name args  =
  let arg = function
    | (k, Cobject cclass) -> sprint "$as(%s, %s)" k (class_name cclass)
    | (k, _) -> k
  in
  let actual = String.concat "," (List.map arg args) in
  print o "#define $%s(%s) %s(%s)" name (arg_list args) name actual

let print_function o ret name args proxy body =
  print o "%s%s(%s)%s"
    (ctype_opt ret) name (param_list args) (if body = None then ";" else "");
  begin match body with
    | None -> ()
    | Some xs ->
      o "{";
      List.iter o xs;
      o "}";
  end;
  if proxy then print_proxy o name args

let print_method o ret cl name args proxy body =
  print_function o ret (method_name cl name) (add_self cl args) proxy body

let methods_defined = function
  | Method (name, _, _, _, _) -> [name]
  | Variable (name, Cobject _) -> ["set_" ^ name]
  | Collection (name, _) -> [on_ name "disconnect"]
  | Slot (name, _) -> [on_ name "disconnect"]
  | Port (name, _) -> [on_ name "disconnect"]
  | _ -> []

let lookup_method name cl_main =
  let pred field = List.mem name (methods_defined field) in
  let rec aux cl =
    match List.find pred (I.class_fields cl) with
    | Method (_, args, ret, _, _) -> (ret, args)
    | Variable (_, Cobject typ) -> (None, ["val", Cobject typ])
    | Collection (_, port) -> (None, ["object", Cobject (I.port_target port)])
    | Slot (name', port) -> (None, ["object", Cobject (I.port_target port)])
    | Port (_, _) -> (None, [])
    | _ -> assert false
    | exception Not_found ->
      match I.class_extend cl with
      | Some cl' -> aux cl'
      | None ->
        Printf.ksprintf failwith
          "class %s has no method %s to override" (class_name cl_main) name
  in
  aux cl_main

let lookup_override name =
  let pred = function Override (name', _) -> name = name' | _ -> false in
  let rec aux cl =
    match List.find pred (I.class_fields cl) with
    | Override _ -> cl
    | _ -> assert false
    | exception Not_found ->
      match I.class_extend cl with
      | Some cl' -> aux cl'
      | None -> raise Not_found
  in
  aux

let lookup_parent_override name cl =
  match I.class_extend cl with
  | None -> raise Not_found
  | Some cl -> lookup_override name cl

let print_class_hierarchy o cl_main =
  let cname = class_name cl_main in
  print o "GOO_CLASS_HIERARCHY(%s)" cname;
  o "{";
  print o "  GOO_CLASS_HIERARCHY_INIT(%s);" cname;
  let rec print_inherit = function
    | None -> ()
    | Some cl ->
      print o "  GOO_CLASS_INHERIT(%s);" (class_name cl);
      print_inherit (I.class_extend cl)
  in
  print_inherit (I.class_extend cl_main);
  o "};";
  o ""

let print_class_methods o cl_main =
  let cname = class_name cl_main in
  print o "GOO_CLASS_METHODS(%s)" cname;
  o "{";
  print o "  GOO_CLASS_METHODS_INIT(%s);" cname;
  let print_method name args ret =
    print o "  %s(* const %s) (%s);"
      (ctype_opt ret) name (param_self_list cl_main args)
  in
  iter_all_fields cl_main (fun _ -> function
      | Method (name, args, ret, _, false) ->
        print_method name args ret;
      | Variable (name, Cobject cclass) ->
        print_method ("set_"^name) ["val", Cobject cclass] None;
      | Collection (name, p) ->
        print o "  GOO_COLLECTION_METHODS(%s, %s, %s);"
          cname name (class_name (I.port_target p))
      | Slot (name, p) ->
        print o "  GOO_SLOT_METHODS(%s, %s, %s);"
          cname name (class_name (I.port_target p))
      | Port (name, _) ->
        print o "  GOO_PORT_METHODS(%s, %s);" cname name
      | _ -> ()
    );
  o "};";
  o ""

let print_class_fields o cl_main =
  let cname = class_name cl_main in
  print o "GOO_CLASS_FIELDS(%s)" cname;
  o "{";
  print o "  GOO_CLASS_FIELDS_INIT(%s);" cname;
  iter_all_fields cl_main (fun cl -> function
      | Variable (name, typ) ->
        let name = match typ with
          | Cobject _ -> "const " ^ name
          | _ -> name
        in
        print o " %s;" (ctype name typ)
      | Collection (name, _) ->
        print o "  goo_collection %s;" name
      | Slot (name, p) ->
        print o "  %s;" (ctype name (Cobject (I.port_target p)))
      | Port (name, _) ->
        print o "  goo_port %s;" name
      | _ -> ()
    );
  o "};";
  o ""

let print_class_method_prototypes o cl =
  List.iter (function
      | Method (name, args, ret, _, _) ->
        print_method o ret cl name args true None;
        o "";
      | Variable (name, Cobject cclass) ->
        print_method o None cl ("set_" ^ name) ["val", Cobject cclass] true None;
        o "";
      | Event (name, args) ->
        print_method o (Some bool) cl ("on_" ^ name) args true None;
        o "";
      | Override (name, _) ->
        let ret, args = lookup_method name cl in
        print_method o ret cl name args true None;
        o "";
      | Constructor (name, args, _) ->
        print_function o (Some (Cobject cl)) (method_name cl name) args true None;
        o "";
      | Collection (name, port) ->
        let target = Cobject (I.port_target port) in
        print_method o None cl (on_ name "disconnect") ["object", target] true None;
        o "";
      | Slot (name, port) ->
        let target = Cobject (I.port_target port) in
        print_method o None cl (on_ name "disconnect") ["object", target] true None;
        o "";
      | Port (name, _) ->
        print_method o None cl (on_ name "disconnect") [] true None;
        o "";
      | _ -> ()
    ) (I.class_fields cl)

let has_constructor cl =
  List.exists (function
      | Constructor _ -> true
      | _ -> false
    ) (I.class_fields cl)

let print_package_h pkg o =
  o "#include \"goo_system.h\"";
  o "";
  (* Forward declare classes. *)
  List.iter (fun c -> print o "GOO_CLASS_DECLARE(%s);" (class_name c))
    (I.package_classes pkg);
  (* Declare enums *)
  List.iter (fun e ->
      o "typedef enum {";
      List.iter (print o "  %s,") (I.enum_members e);
      print o "} %s;" (enum_name e);
      o "";
    ) (I.package_enums pkg);
  o "";
  List.iter o (I.package_declarations pkg);
  o "";
  (* Declare functions *)
  List.iter (fun f -> print_function o f.I.fn_ret (I.mangle pkg f.I.fn_name) f.I.fn_args true None)
    (I.package_funcs pkg);
  o "";
  (* Declare classes *)
  List.iter (fun clss ->
      print_class_methods o clss;
      print_class_fields o clss;
      print_class_hierarchy o clss;
      print_class_method_prototypes o clss
    ) (I.package_classes pkg);
  (* Declare relations *)
  List.iter (fun cl ->
      let cname = class_name cl in
      List.iter (function
          | Collection (name, port) ->
            print o "GOO_COLLECTION(%s, %s, %s);"
              cname name (class_name (I.port_target port))
          | Slot (name, port) ->
            print o "GOO_SLOT(%s, %s, %s);"
              cname name (class_name (I.port_target port))
          | Port (name, port) ->
            print o "GOO_PORT(%s, %s, %s);"
              cname name (class_name (I.port_source port))
          | _ -> ()
        ) (I.class_fields cl)
    ) (I.package_classes pkg)

let iter_methods_to_impl cl f =
  List.iter (function
      | Method (name, args, ret, body, _) ->
        f (ctype_opt ret) name (param_self_list cl args) body
      | Override (name, body) ->
        let ret, args = lookup_method name cl in
        f (ctype_opt ret) name (param_self_list cl args) body
      | Constructor (name, args, body) ->
        f (ctype "" (Cobject cl)) name (param_list args) body
      | _ -> ()
    ) (I.class_fields cl)

let print_class_impl_h cl_main o =
  print o "#include \"%s.h\"" (I.package_name (I.class_package cl_main));
  o "#include \"ml_goo.h\"";
  o "";
  let print_collection_method cl name args =
    match lookup_override name cl_main with
    | latest when latest != cl_main ->
      print o "#define self_%s %s" name (method_name latest name);
    | _ ->
      let args = add_self cl_main args in
      print_function o None (method_name cl_main name) args false None;
      print o "#define self_%s %s" name (method_name cl_main name);
      (match lookup_parent_override name cl_main with
       | previous ->
         print o "#define super_%s %s" name (method_name previous name);
       | exception Not_found -> ()
      )
    | exception Not_found when cl_main == cl ->
      let args = add_self cl_main args in
      print_function o None (method_name cl_main name) args false (Some []);
      print o "#define self_%s %s" name (method_name cl name);
    | exception Not_found ->
      print o "#define self_%s %s" name (method_name cl name);
  in
  let print_field cl = function
    | Method (name, args, ret, _, _) ->
      let latest = try lookup_override name cl_main with Not_found -> cl in
      print o "#define self_%s %s" name (method_name latest name);
      if latest == cl_main then (
        let args = add_self cl_main args in
        print_function o ret (method_name cl_main name) args false None;
        match I.class_extend latest with
        | None -> ()
        | Some super ->
          let previous = try lookup_override name super with Not_found -> cl in
          print o "#define super_%s %s" name (method_name previous name)
      )
    | Variable (field_name, Cobject typ) ->
      let name = "set_" ^ field_name in
      print o "#define self_%s %s" name (method_name cl name);
      begin match lookup_override name cl_main with
        | latest when latest != cl_main -> ()
        | latest ->
          print_function o None (method_name cl_main name)
            (add_self cl ["arg", Cobject typ]) false None;
          if cl_main == cl then (
            print o "static void super_%s(%s *self, %s *val)"
              name (class_name cl_main) (class_name typ);
            print o "{ *(%s **)(&$field(self, %s)) = val; $ml_goo_set_property(self, %d, val); }"
              (class_name typ) field_name (property_index cl_main field_name);
            print_proxy o ("super_" ^ name) (add_self cl_main ["val", Cobject typ])
          );
          (match lookup_parent_override name latest with
           | exception Not_found -> ()
           | previous ->
             print o "#define super_%s %s_%s" name (class_name previous) name
          )
        | exception Not_found when cl_main == cl ->
          let args = add_self cl ["val", cobject typ] in
          print_function o None (method_name cl name) args false (Some [
              sprint "  *(%s **)(&$field(self, %s)) = val;"
                (class_name typ) field_name;
              sprint "  $ml_goo_set_property(self, %d, val);"
                (property_index cl_main field_name);
            ]);
        | exception Not_found -> ()
      end
    | Event (name, args) ->
      print o "#define self_on_%s %s" name (method_name cl ("on_" ^ name));
      (*print_proxy o ("self_on_" ^ name) (add_self cl args);*)
    | Collection (name, port) ->
      let target = Cobject (I.port_target port) in
      print_collection_method cl (on_ name "disconnect") ["object", target];
      print o "GOO_INTERNAL_COLLECTION(%s, %s, %d, %s, %s, %d);"
        (class_name cl) name (property_index cl name)
        (class_name (I.port_target port)) (I.port_name port)
        (property_index (I.port_target port) (I.port_name port));
    | Slot (name, port) ->
      let target = Cobject (I.port_target port) in
      print_collection_method cl (on_ name "disconnect") ["object", target];
      print o "GOO_INTERNAL_SLOT(%s, %s, %d, %s, %s, %d);"
        (class_name cl) name (property_index cl name)
        (class_name (I.port_target port)) (I.port_name port)
        (property_index (I.port_target port) (I.port_name port));
    | Port (name, port) ->
      print_collection_method cl (on_ name "disconnect") []
    | _ -> ()
  in
  iter_all_fields cl_main print_field;
  o "";
  if has_constructor cl_main then (
    print o "GOO_INTERNAL_DISPLAY(%s, %d)"
      (class_name cl_main) (I.class_depth cl_main);
    o "{";
    print o "  GOO_INTERNAL_DISPLAY_INIT(%s, %d, %d)"
      (class_name cl_main) (I.class_depth cl_main) (number_of_properties cl_main);
    let rec witnesses acc c =
      let name = "&goo_" ^ class_name c ^ "_witness" in
      match I.class_extend c with
      | None -> name :: acc
      | Some c -> witnesses (name :: acc) c
    in
    print o "  {%s}" (String.concat ", " (witnesses [] cl_main));
    o "};";
    o "";
    print o "GOO_INTERNAL_TABLE(%s)" (class_name cl_main);
    o "{";
    print o "  GOO_INTERNAL_TABLE_INIT(%s)," (class_name cl_main);
    iter_all_fields cl_main (fun _ -> function
        | Method (name, _, _, _, false) ->
          print o "  GOO_INTERNAL_TABLE_METHOD(%s)," name
        | Variable (name, Cobject _) ->
          print o "  GOO_INTERNAL_TABLE_METHOD(set_%s)," name
        | Collection (name, _) ->
          print o "  GOO_INTERNAL_TABLE_COLLECTION(%s)," name
        | Slot (name, _) ->
          print o "  GOO_INTERNAL_TABLE_SLOT(%s)," name
        | Port (name, _) ->
          print o "  GOO_INTERNAL_TABLE_PORT(%s)," name
        | _ -> ()
      );
    o "};";
  ) else (
    print o "GOO_INTERNAL_WITNESS(%s, %d);"
      (class_name cl_main) (I.class_depth cl_main);
  );
  o "";
  o "/* Methods to implement */";
  iter_methods_to_impl cl_main (fun cret name cargs -> function
      | None -> print o "%s %s(%s);" cret (method_name cl_main name) cargs
      | Some body ->
        print o "%s %s(%s)" cret (method_name cl_main name) cargs;
        o "{";
        List.iter o (body cl_main);
        o "}"
    )

let print_class_impl_c cl o =
  print o "#include \"%s.h\"" (class_name cl);
  o "";
  iter_methods_to_impl cl (fun cret name cargs body ->
      if body = None then (
        print o "%s self_%s(%s)" cret name cargs;
        o "{";
        o "  /* TODO */";
        o "}"
      )
    )

let guard_header o n f =
  let n = String.uppercase_ascii n in
  print o "#ifndef __%s_H__" n;
  print o "#define __%s_H__" n;
  o "";
  let result = f o in
  o "";
  print o "#endif /* !__%s_H__ */" n;
  result

let with_file ~force name f =
  if Sys.file_exists name && not force then () else
    let oc = open_out name in
    let last_blank = ref true in
    let output_line line =
      let blank = line = "" in
      if not (!last_blank && blank) then (
        output_string oc line;
        output_char oc '\n'
      );
      last_blank := blank
    in
    try
      let r = f output_line in
      close_out_noerr oc;
      r
    with exn ->
      close_out_noerr oc;
      raise exn

let generate pkg ~dir =
  let rec mkdir path =
    if Sys.file_exists path then () else
      let dir, name = Filename.dirname path, Filename.basename path in
      if dir = path then () else mkdir dir;
      (try Unix.mkdir name 0o777 with _ -> ())
  in
  mkdir dir;
  let name = I.package_name pkg in
  let filename base ext = Filename.concat dir (base ^ "." ^ ext) in
  with_file (filename name "h") ~force:true (fun o ->
      guard_header o
        (String.uppercase_ascii (I.package_name pkg) ^ "_CLASSES")
        (print_package_h pkg));
  List.iter
    (fun c -> with_file ~force:true (filename (class_name c) "h") (print_class_impl_h c))
    (I.package_classes pkg);
  List.iter
    (fun c -> with_file ~force:false (filename (class_name c) "c") (print_class_impl_c c))
    (I.package_classes pkg);
  ()
