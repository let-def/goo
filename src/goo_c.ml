open Goo_model
module I = Introspect

let class_name cl = I.(name_of (class_package cl) ^ "_" ^ name_of cl)
let enum_name en = I.(name_of (enum_package en) ^ "_" ^ name_of en)

let is_abstract, set_concrete =
  let table : (classe, unit) I.Table.table = I.Table.create () in
  (fun cl -> not (I.Table.mem table cl)),
  (fun cl -> I.Table.add table cl ())

let get_list table key =
  try I.Table.find table key with Not_found ->
    let l = ref [] in
    I.Table.add table key l;
    l

let add_to_list l v = l := v :: !l

let is_dynamic, set_dynamic =
  let table : (func, unit) I.Table.table = I.Table.create () in
  let assert_method func = match I.func_kind func with
    | I.Fn_class cl ->
      begin match I.func_args func with
        | ("self", Object cl') :: _ when cl = cl' -> ()
        | _ ->
          Printf.ksprintf failwith
            "Goo_c.dynamic: %s.%s to be a dynamic method, first argument should be a receiver 'self' of class %s"
            (class_name cl) (I.name_of func) (class_name cl)
      end
    | I.Fn_package pkg ->
      Printf.ksprintf failwith "Goo_c.dynamic: %s.%s is not a class method"
        (I.name_of pkg) (I.name_of func)
  in
  (fun func -> I.Table.mem table func),
  (fun func -> assert_method func; I.Table.add table func ())

type variable_desc = { v_type : ctype }
type variable = variable_desc id

let instance_variables, instance_variable =
  let table : (classe, variable list ref) I.Table.table = I.Table.create () in
  (fun cl -> List.rev !(get_list table cl)),
  (fun cl name v_type ->
     add_to_list (get_list table cl) (Goo_id.inj name { v_type }))

let override, overriden =
  let table : (classe, func list ref) I.Table.table = I.Table.create () in
  (fun cl func -> add_to_list (get_list table cl) func),
  (fun cl -> List.rev !(get_list table cl))

let get_disconnect_callback, add_disconnect_callback =
  let table : (I.class_relation, func) Hashtbl.t = Hashtbl.create 7 in
  (fun rel -> match Hashtbl.find table rel with x -> Some x | exception Not_found -> None),
  (fun rel func -> not (Hashtbl.mem table rel) && (Hashtbl.add table rel func; true))

let on_port_disconnect pt cb =
  if not (add_disconnect_callback (I.Rel_port pt) cb) then
    Printf.ksprintf failwith
      "port %s already has a disconnect callback registered"
      (I.name_of pt)

let on_slot_disconnect pt cb =
  if not (add_disconnect_callback (I.Rel_slot pt) cb) then
    Printf.ksprintf failwith
      "port %s already has a disconnect callback registered"
      (I.name_of pt)

let on_collection_disconnect pt cb =
  if not (add_disconnect_callback (I.Rel_collection pt) cb) then
    Printf.ksprintf failwith
      "port %s already has a disconnect callback registered"
      (I.name_of pt)

let rec lookup_override cl func =
  let parent = match I.class_extend cl with
    | Some cl -> lookup_override cl func
    | None ->
      match I.func_kind func with
      | I.Fn_class cl -> [cl]
      | _ -> assert false
  in
  if List.mem func (overriden cl) then
    cl :: parent
  else
    parent

let () = set_dynamic Goo_model.goo_destroy

let package_declare, package_get_declarations =
  let table : (package, string list ref) I.Table.table = I.Table.create () in
  (fun pkg decls -> match I.Table.find table pkg with
     | lines -> lines := List.rev_append decls !lines
     | exception Not_found ->
       I.Table.add table pkg (ref (List.rev decls))
  ),
  (fun pkg -> match I.Table.find table pkg with
     | lines -> List.rev !lines
     | exception Not_found -> [])

let sprint = Printf.sprintf
let print o fmt = Printf.ksprintf o fmt
let on_ x y = "on_" ^ x ^ "_" ^ y

let ctype ident = function
  | Bool      -> sprint "goo_bool %s" ident
  | Int       -> sprint "int %s" ident
  | Float     -> sprint "double %s" ident
  | String    -> sprint "goo_string %s" ident
  | Flag en   -> sprint "%s %s" (enum_name en) ident
  | Object cl -> sprint "%s *%s" (class_name cl) ident
  | Object_option cl -> sprint "goo_option %s *%s" (class_name cl) ident
  | Custom s  -> sprint "%s%s%s" s (if s.[String.length s - 1] <> ' ' then " " else "") ident

let iter_ancestors ?(and_self=false) cl f =
  let rec aux = function
    | None -> ()
    | Some cl -> aux (I.class_extend cl); f cl
  in
  aux (I.class_extend cl);
  if and_self then f cl

let number_of_properties cl0 =
  let count = ref 0 in
  iter_ancestors ~and_self:true cl0 (fun cl ->
      List.iter (fun var ->
          match (Goo_id.prj var ).v_type with
          | Object _ | Object_option _ -> incr count
          | _ -> ())
        (instance_variables cl);
      List.iter (fun _event -> incr count)
        (I.class_events cl);
      List.iter (function
          | I.Rel_collection _ | I.Rel_slot _ -> incr count
          | I.Rel_port _ -> count := !count + 2)
        (I.class_relations cl);
    );
  !count

let property_index cl0 name =
  let count = ref 0 in
  match iter_ancestors ~and_self:true cl0 (fun cl ->
      List.iter (fun var ->
          match (Goo_id.prj var).v_type with
          | Object _ | Object_option _ ->
            if I.name_of var = name then raise Exit;
            incr count
          | _ -> ())
        (instance_variables  cl);
      List.iter (fun event ->
          if I.name_of event = name then raise Exit
          else incr count)
        (I.class_events cl);
      List.iter (function
          | I.Rel_collection x when I.name_of x = name -> raise Exit
          | I.Rel_slot x when I.name_of x = name -> raise Exit
          | I.Rel_port x when I.name_of x = name -> raise Exit
          | I.Rel_collection _ | I.Rel_slot _ -> incr count
          | I.Rel_port _ -> count := !count + 2)
        (I.class_relations cl);
    )
  with
  | () -> raise Not_found
  | exception Exit -> !count

let func_symbol ?at_class func =
  let prefix =
    match at_class, I.func_kind func with
    | Some cl, _ | None, I.Fn_class cl -> class_name cl
    | None, I.Fn_package pkg -> I.name_of pkg
  in
  prefix ^ "_" ^ I.name_of func

let params_str params =
  String.concat ", " (List.map (fun (n,ty) -> ctype n ty) params)

let func_params ?at_class func = match I.func_args ?at_class func, I.func_ret func with
  | [], ([] | [_]) -> []
  | params, ((_ :: ([] as ret)) | ret) ->
    params @ List.mapi (fun i ty -> ("*ret" ^ string_of_int i), ty) ret

let func_params_str ?at_class func = match func_params ?at_class func with
  | [] -> "void"
  | params -> params_str params

let func_ret func = match I.func_ret func with
  | [typ] -> Some typ
  | [] | (_ :: _ :: _) -> None

let func_ret_str func = match func_ret func with
  | Some typ -> ctype "" typ
  | None -> "void "

let func_args_str ?at_class func =
  let prepare_arg (k, _) =
    if k.[0] = '*' then String.sub k 1 (String.length k - 1) else k
  in
  String.concat ", " (List.map prepare_arg (func_params ?at_class func ))

let print_proxy o name params =
  let remove_star k =
    if k.[0] = '*' then String.sub k 1 (String.length k - 1) else k
  in
  let prepare_formal (k, _) = remove_star k in
  let prepare_actual = function
    | (k, Object cl) when k.[0] <> '*' ->
      sprint "$as(%s, %s)" k (class_name cl)
    | (k, _) -> remove_star k
  in
  let formal = String.concat "," (List.map prepare_formal params) in
  let actual = String.concat "," (List.map prepare_actual params) in
  print o "#define $%s(%s) %s(%s)" name formal name actual

let print_function o ?at_class func proxy body =
  print o "%s%s(%s)%s"
    (func_ret_str func) (func_symbol func) (func_params_str ?at_class func)
    (if body = None then ";" else "");
  begin match body with
    | None -> ()
    | Some xs ->
      o "{";
      List.iter o xs;
      o "}";
  end;
  if proxy then
    print_proxy o (func_symbol func) (func_params ?at_class func)

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
  iter_ancestors ~and_self:true cl_main
    (fun cl ->
       List.iter (fun func ->
           if is_dynamic func then
             print o "  %s( *const %s ) (%s);"
               (func_ret_str func) (I.name_of func)
               (func_params_str ~at_class:cl_main func)
         ) (I.class_funcs cl)
    );
  o "};";
  o ""

let print_class_fields o cl_main =
  let cname = class_name cl_main in
  print o "GOO_CLASS_FIELDS(%s)" cname;
  o "{";
  print o "  GOO_CLASS_FIELDS_INIT(%s);" cname;
  iter_ancestors ~and_self:true cl_main
    (fun cl ->
       List.iter (fun var ->
           let name = I.name_of var and typ = (Goo_id.prj var).v_type in
           let name = match typ with
             | Object _ | Object_option _ -> "const " ^ name
             | _ -> name
           in
           print o "  %s;" (ctype name typ)
         ) (instance_variables  cl);
       List.iter (function
           | I.Rel_collection col ->
             print o "  goo_collection %s;" (I.name_of col)
           | I.Rel_slot sl ->
             let pt = I.slot_port sl in
             print o "  %s;" (ctype ("const " ^ I.name_of sl)
                                (Object_option (I.port_target pt)))
           | I.Rel_port pt ->
             print o "  goo_port %s;" (I.name_of pt)
         ) (I.class_relations cl);
    );
  o "};";
  o ""

let print_class_method_prototypes o cl =
  List.iter (fun func -> print_function o func true None) (I.class_funcs cl);
  List.iter (fun func ->
      if is_dynamic func then
        print o "%sstatic_%s(%s);"
          (func_ret_str func) (func_symbol ~at_class:cl func) (func_params_str ~at_class:cl func)
      else
        print o "#define static_%s %s" (func_symbol ~at_class:cl func) (func_symbol ~at_class:cl func)
    )
    (overriden cl @ I.class_funcs cl)

let print_package_h pkg o =
  o "#include \"goo_system.h\"";
  o "";
  (* Forward declare classes. *)
  List.iter (fun c -> print o "GOO_CLASS_DECLARE(%s);" (class_name c))
    (I.package_classes pkg);
  o "";
  (* Declare enums *)
  List.iter (fun e ->
      o "typedef enum {";
      List.iter (fun m -> print o "  %s," (I.name_of m)) (I.enum_members e);
      print o "} %s;" (enum_name e);
      o "";
    ) (I.package_enums pkg);
  o "";
  List.iter o (package_get_declarations pkg);
  o "";
  (* Declare functions *)
  List.iter
    (fun func -> print_function o func true None)
    (I.package_funcs pkg);
  (* Declare classes *)
  List.iter (fun cl ->
      o "";
      print_class_methods o cl;
      print_class_fields o cl;
      print_class_hierarchy o cl;
      print_class_method_prototypes o cl
    ) (I.package_classes pkg);
  o "";
  (* Declare relations *)
  List.iter (fun cl ->
      let cname = class_name cl in
      List.iter (function
          | I.Rel_collection col ->
            let pt = I.collection_port col in
            print o "GOO_COLLECTION(%s, %s, %s);"
              cname (I.name_of col) (class_name (I.port_target pt))
          | I.Rel_slot sl ->
            let pt = I.slot_port sl in
            print o "GOO_SLOT(%s, %s, %s);"
              cname (I.name_of sl) (class_name (I.port_target pt))
          | I.Rel_port pt ->
            print o "GOO_PORT(%s, %s, %s);"
              cname (I.name_of pt) (class_name (I.port_source pt))
        ) (I.class_relations cl)
    ) (I.package_classes pkg)

let print_class_impl_h cl o =
  print o "#include \"%s.h\"" (I.name_of (I.class_package cl));
  o "#include \"ml_goo.h\"";
  o "";
  o "/* Methods to implement */";
  o "";
  (* Overriden methods *)
  iter_ancestors cl (fun cl' ->
      List.iter
        (fun func ->
           match lookup_override cl func with
           | cl0 :: _ when cl0 = cl ->
             print o "$method %sself_%s(%s);"
               (func_ret_str func) (I.name_of func) (func_params_str ~at_class:cl func)
           | _ -> ()
        )
        (I.class_funcs cl')
    );
  (* New methods *)
  List.iter
    (fun func ->
       print o "$method %sself_%s(%s);"
         (func_ret_str func) (I.name_of func) (func_params_str func))
    (I.class_funcs cl);
  o "";
  o "/* Internal definitions */";
  o "";
  iter_ancestors cl (fun cl' ->
      List.iter
        (fun func ->
           let overrides = lookup_override cl func in
           let next_cl, next_prefix =
             match overrides with
             | cl0 :: cl1 :: _ when cl0 = cl ->
               let sym = func_symbol ~at_class:cl func in
               let ret_type = func_ret_str func in
               let ret_call = if func_ret func = None then "" else "return " in
               let params = func_params_str ~at_class:cl func in
               let args = func_args_str ~at_class:cl func in
               print o "#define static_self_%s self_%s" (I.name_of func) (I.name_of func);
               print o "#define $static_self_%s $%s" (I.name_of func) sym;
               print o "%sstatic_%s(%s) { %sself_%s(%s); }"
                 ret_type sym params ret_call (I.name_of func) args;
               cl1, "super"
             | cl0 :: _ -> cl0, "self"
             | [] -> assert false
           in
           let sym = func_symbol ~at_class:next_cl func in
           print o "#define static_%s_%s static_%s"
             next_prefix (I.name_of func) sym;
           print o "#define $static_%s_%s $%s"
             next_prefix (I.name_of func) sym
        )
        (I.class_funcs cl')
    );
  List.iter
    (fun func ->
       let ret_type = func_ret_str func in
       let ret_call = if func_ret func = None then "" else "return " in
       print o "#define static_self_%s self_%s" (I.name_of func) (I.name_of func);
       print o "#define $static_self_%s $%s" (I.name_of func) (func_symbol func);
       if is_dynamic func then
         print o "%s%s(%s) { %s$send(self, %s)(%s); }"
           ret_type (func_symbol func) (func_params_str func)
           ret_call (I.name_of func) (func_args_str func);
       print o "%sstatic_%s(%s) { %sself_%s(%s); }"
         ret_type (func_symbol func) (func_params_str func)
         ret_call (I.name_of func) (func_args_str func)
    )
    (I.class_funcs cl);
  o "";
  o "/* Relations */";
  o "";
  let rel_name_and_index cl id =
    (class_name cl, I.name_of id, property_index cl (I.name_of id)) in
  List.iter (fun rel ->
      let cb = get_disconnect_callback rel in
      match rel with
      | I.Rel_collection col ->
        let pt = I.collection_port col in
        let target = I.port_target pt in
        let src_cl, src_name, src_id = rel_name_and_index cl col in
        let dst_cl, dst_name, dst_id = rel_name_and_index target pt in
        print o "#define $port_%s_disconnect %s" src_name
          (match cb with
           | None -> sprint "(void(*)(%s *, %s *))NULL" src_cl dst_cl
           | Some func -> func_symbol func);
        print o "GOO_INTERNAL_COLLECTION(%s, %s, %d, %s, %s, %d);"
          src_cl src_name src_id
          dst_cl dst_name dst_id;
        print_proxy o ("static_connect_" ^ I.name_of col)
          ["self", Object cl; "that", Object target; "after_that", Object_option target]
      | I.Rel_slot sl ->
        let pt = I.slot_port sl in
        let target = I.port_target pt in
        let src_cl, src_name, src_id = rel_name_and_index cl sl in
        let dst_cl, dst_name, dst_id = rel_name_and_index target pt in
        print o "#define $port_%s_disconnect %s" src_name
          (match cb with
           | None -> sprint "(void(*)(%s *, %s *))NULL" src_cl dst_cl
           | Some func -> func_symbol func);
        print o "GOO_INTERNAL_SLOT(%s, %s, %d, %s, %s, %d);"
          src_cl src_name src_id
          dst_cl dst_name dst_id;
        print_proxy o ("static_connect_" ^ I.name_of sl)
          ["self", Object cl; "item", Object target]
      | I.Rel_port pt ->
        print o "#define $port_%s_disconnect(object) %s(object)" (I.name_of pt)
          (match cb with
           | None -> "(void)"
           | Some func -> func_symbol func);
        print o "GOO_INTERNAL_PORT(%s, %s, %s);"
          (class_name (I.port_target pt)) (I.name_of pt)
          (class_name (I.port_source pt))
    ) (I.class_relations cl);
  o "";
  o "/* Heap variable setters */";
  o "";
  List.iter (fun var ->
      let name = I.name_of var and typ = (Goo_id.prj var).v_type in
      match typ with
      | Object arg | Object_option arg ->
        print o "static inline void static_set_%s(%s *self, %s *v)" name
          (class_name cl) (class_name arg);
        o "{";
        print o "  *(%s ** )(&$field(self, %s)) = val;" (class_name arg) name;
        print o "  $ml_goo_set_property(self, %d, val);" (property_index cl name);
        o "}";
        print_proxy o ("static_set_" ^ name) ["self", Object cl; "v", typ]
      | _ -> ()
    ) (instance_variables cl);
  o "";
  o "/* Events */";
  o "";
  iter_ancestors ~and_self:true cl
    (fun cl ->
       List.iter (fun event ->
           let args = arg "self" (Object (I.event_classe event)) :: I.event_args event in
           print o "goo_bool event_%s_%s(%s);"
             (class_name cl) (I.name_of event) (params_str args);
           print o "#define $static_event_%s event_%s_%s"
             (I.name_of event) (class_name cl) (I.name_of event);
           print_proxy o
             (sprint "event_%s_%s" (class_name cl) (I.name_of event)) args
         ) (I.class_events cl)
    );
  (*let print_field cl = function
    | Event (name, args) ->
      print o "#define self_on_%s %s" name (method_name cl ("on_" ^ name));
      (*print_proxy o ("self_on_" ^ name) (add_self cl args);*)
    *)
  o "";
  o "/* Class definition */";
  o "";
  if is_abstract cl then (
    print o "GOO_INTERNAL_WITNESS(%s, %d);"
      (class_name cl) (I.class_depth cl);
  ) else (
    print o "GOO_INTERNAL_DISPLAY(%s, %d)"
      (class_name cl) (I.class_depth cl);
    o "{";
    print o "  GOO_INTERNAL_DISPLAY_INIT(%s, %d, %d)"
      (class_name cl) (I.class_depth cl) (number_of_properties cl);
    let rec witnesses acc c =
      let name = "&goo_" ^ class_name c ^ "_witness" in
      match I.class_extend c with
      | None -> name :: acc
      | Some c -> witnesses (name :: acc) c
    in
    print o "  {%s}" (String.concat ", " (witnesses [] cl));
    o "};";
    o "";
    print o "GOO_INTERNAL_TABLE(%s)" (class_name cl);
    o "{";
    print o "  GOO_INTERNAL_TABLE_INIT(%s)," (class_name cl);
    iter_ancestors ~and_self:true cl
      (fun cl ->
         List.iter
           (fun func ->
              if is_dynamic func then
                print o "  GOO_INTERNAL_TABLE_METHOD(%s)," (I.name_of func))
           (I.class_funcs cl)
      );
    o "};";
  )

let print_class_impl_c cl o =
  print o "#include \"%s.h\"" (class_name cl);
  o ""

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
  let file_exists = Sys.file_exists name in
  if file_exists && not force then () else
    let name' = if file_exists then "." ^ name else name in
    let oc = open_out name' in
    let last_blank = ref true in
    let output_line line =
      let blank = line = "" in
      if not (!last_blank && blank) then (
        output_string oc line;
        output_char oc '\n'
      );
      last_blank := blank
    in
    let cleanup () =
      close_out_noerr oc;
      if file_exists then (
        let hash = Digest.file name in
        let hash' = Digest.file name' in
        if Digest.equal hash hash' then
          Sys.remove name'
        else
          Sys.rename name' name
      )
    in
    try
      let r = f output_line in
      cleanup ();
      r
    with exn ->
      cleanup ();
      raise exn

let generate pkg ~dir =
  let rec mkdir path =
    if Sys.file_exists path then () else
      let dir, name = Filename.dirname path, Filename.basename path in
      if dir = path then () else mkdir dir;
      (try Unix.mkdir name 0o777 with _ -> ())
  in
  mkdir dir;
  let filename base ext = Filename.concat dir (base ^ "." ^ ext) in
  with_file (filename (I.name_of pkg) "h") ~force:true (fun o ->
      guard_header o
        (String.uppercase_ascii (I.name_of pkg) ^ "_CLASSES")
        (print_package_h pkg));
  List.iter
    (fun c -> with_file ~force:true (filename (class_name c) "h") (print_class_impl_h c))
    (I.package_classes pkg);
  List.iter
    (fun c -> with_file ~force:false (filename (class_name c) "c") (print_class_impl_c c))
    (I.package_classes pkg);
  ()
