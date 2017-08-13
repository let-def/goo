open Goo_model
module C = Goo_c
module I = Introspect

let sprint = Printf.sprintf
let print o fmt = Printf.ksprintf o fmt

let qualify_pkg (pkg : package) path_pkg path =
  if pkg = path_pkg then path else
    sprint "%s.%s" (String.capitalize_ascii (I.name_of path_pkg)) path

let class_name pkg cl =
  let cl_pkg = I.class_package cl in
  if cl = Goo_model.goo_object then
    "goo_object"
  else
    qualify_pkg pkg cl_pkg (I.name_of cl)

let enum_name pkg en =
  qualify_pkg pkg (I.enum_package en) (I.name_of en)

let c_func_name func = C.func_symbol func

exception Not_an_ml_type

let mltype pkg = function
  | Bool   -> "bool"
  | Int    -> "int"
  | Float  -> "float"
  | String -> "string"
  | Flag e -> enum_name pkg e
  | Object t -> sprint "[> %s] goo" (class_name pkg t)
  | Object_option t -> sprint "[> %s] goo option" (class_name pkg t)
  | Custom _  -> raise Not_an_ml_type

let ml_function pkg ?(allow_caf=false) args ret =
  let mltype_ret = function
    | (Object t | Object_option t as x) ->
      let opt = match x with Object_option _ -> " option" | _ -> "" in
      sprint "%s goo%s" (class_name pkg t) opt
    | x -> mltype pkg x
  in
  let rec aux acc = function
    | [] ->
      List.rev
        ((if ret = [] then "unit" else String.concat " * " (List.map mltype_ret ret)) :: acc)
    | (_, typ) :: xs -> aux (mltype pkg typ :: acc) xs
  in
  let args = match aux [] args with
    | [ret] when not allow_caf -> ["unit"; ret]
    | args -> args
  in
  String.concat " -> " args

let print_ml_stubs pkg o =
  o "open Goo";
  o "";
  List.iter (fun e ->
      print o "type %s = [" (enum_name pkg e);
      List.iter
        (fun x -> print o "  | `%s" (I.name_of x))
        (I.enum_members e);
      o "]";
    ) (I.package_enums pkg);
  List.iter (fun c ->
      begin match I.class_extend c with
        | None -> print o "type %s = [`%s]" (I.name_of c) (class_name pkg c)
        | Some c' ->
          let cname = class_name pkg c in
          let c'name = class_name pkg c' in
          print o "type %s = [`%s | %s]" cname (C.class_name c) c'name;
      end;
      o ""
    ) (I.package_classes pkg);
  List.iter (fun func ->
      print o "external %s : %s = \"ml_%s\""
        (I.name_of func)
        (ml_function pkg (I.func_args func) (I.func_ret func))
        (c_func_name func)
    ) (I.package_funcs pkg);
  List.iter (fun cl ->
      o "";
      let cname' = I.name_of cl in
      let cname = C.class_name cl in
      print o "external %s_witness : unit -> %s witness = \"ml_witness_%s\"" cname' cname' cname;
      let stub_name name arity =
        if arity > 5 then
          sprint "\"ml_bc_%s\" \"ml_%s\"" name name
        else
          sprint "\"ml_%s\"" name
      in
      List.iter (fun func ->
          try
            print o "external %s_%s : %s = %s"
              (I.name_of cl) (I.name_of func)
              (ml_function pkg (I.func_args func) (I.func_ret func))
              (stub_name (C.func_symbol func) (List.length (I.func_args func)))
          with Not_an_ml_type -> ())
        (I.class_funcs cl);
      List.iter (fun event ->
          print o "let event_%s_%s : ([> %s], %s) event = Obj.magic %d"
            (I.name_of cl) (I.name_of event) (I.name_of cl)
            (ml_function pkg ~allow_caf:true (I.event_args event) (I.event_ret event))
            (Goo_c.property_index cl (I.name_of event))
        ) (I.class_events cl);
      List.iter (function
          | I.Rel_collection col ->
            let name = I.name_of col in
            let prefix' = sprint "%s_%s" cname' name in
            let prefix = sprint "%s_%s" cname name in
            let target = class_name pkg (I.port_target (I.collection_port col)) in
            print o "external %s_prev : [> %s] goo -> %s goo option = \"ml_%s_prev\""
              prefix' target target prefix;
            print o "external %s_next : [> %s] goo -> %s goo option = \"ml_%s_next\""
              prefix' target target prefix;
            print o "external %s_first : [> %s] goo -> %s goo option = \"ml_%s_first\""
              prefix' cname' target prefix;
            print o "external %s_last : [> %s] goo -> %s goo option = \"ml_%s_last\""
              prefix' cname' target prefix;
            print o "external %s_parent : [> %s] goo -> %s goo option = \"ml_%s_parent\""
              prefix' target cname' prefix;
          | I.Rel_slot sl ->
            let name = I.name_of sl in
            let prefix' = sprint "%s_%s" cname' name in
            let prefix = sprint "%s_%s" cname name in
            let target = class_name pkg (I.port_target (I.slot_port sl)) in
            print o "external %s_get : [> %s] goo -> %s goo option = \"ml_%s_get\""
              prefix' cname' target prefix;
          | I.Rel_port pt ->
            let name = I.name_of pt in
            let prefix' = sprint "%s_%s" cname' name in
            let prefix = sprint "%s_%s" cname name in
            print o "external %s_get : [> %s] goo -> %s goo option  = \"ml_%s_get\""
              prefix' cname' (class_name pkg (I.port_source pt)) prefix;
            print o "external %s_detach : [> %s] goo -> unit  = \"ml_%s_detach\""
              prefix' cname' prefix;
        ) (I.class_relations cl)
    ) (I.package_classes pkg)

let n_args n =
  let r = ref [] in
  for i = n - 1 downto 0
  do r := ("value arg" ^ string_of_int i) :: !r done;
  String.concat ", " !r

let rec caml_xparam o i j =
  match j - i with
  | 0 -> ()
  | 1 -> print o "  CAMLxparam1(arg%d);" i
  | 2 -> print o "  CAMLxparam2(arg%d, arg%d);" i (i + 1)
  | 3 -> print o "  CAMLxparam3(arg%d, arg%d, arg%d);" i (i + 1) (i + 2)
  | 4 -> print o "  CAMLxparam4(arg%d, arg%d, arg%d, arg%d);" i (i + 1) (i + 2) (i + 3)
  | n -> print o "  CAMLxparam5(arg%d, arg%d, arg%d, arg%d, arg%d);" i (i + 1) (i + 2) (i + 3) (i + 4);
    caml_xparam o (i + 5) j

let caml_param o = function
  | 0 -> o "  CAMLparam0();"
  | 1 -> o "  CAMLparam1(arg0);"
  | 2 -> o "  CAMLparam2(arg0, arg1);"
  | 3 -> o "  CAMLparam3(arg0, arg1, arg2);"
  | 4 -> o "  CAMLparam4(arg0, arg1, arg2, arg3);"
  | n -> o "  CAMLparam5(arg0, arg1, arg2, arg3, arg4);";
    caml_xparam o 5 n

let rec caml_local o i j =
  match j - i with
  | 0 -> ()
  | 1 -> print o "  CAMLlocal1(var%d);" i
  | 2 -> print o "  CAMLlocal2(var%d, var%d);" i (i + 1)
  | 3 -> print o "  CAMLlocal3(var%d, var%d, var%d);" i (i + 1) (i + 2)
  | 4 -> print o "  CAMLlocal4(var%d, var%d, var%d, var%d);" i (i + 1) (i + 2) (i + 3)
  | n -> print o "  CAMLlocal5(var%d, var%d, var%d, var%d, var%d);" i (i + 1) (i + 2) (i + 3) (i + 4);
    caml_local o (i + 5) j

let caml_local o n = caml_local o 0 n

let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

let bytecode_proxy o name argc =
  print o "value ml_bc_%s(value *argv, int argn)" name;
  o "{";
  print o "  goo_assert (argn = %d);" argc;
  let acc = ref [] in
  for i = argc - 1 downto 0 do
    acc := sprint "argv[%d]" i :: !acc
  done;
  print o "  return ml_%s(%s);"  name (String.concat "," !acc);
  o "}"

let print_ml_c_stubs pkg o =
  o "#include <caml/alloc.h>";
  o "#include <caml/memory.h>";
  o "#include <caml/callback.h>";
  o "#include \"ml_goo.h\"";
  print o "#include \"%s.h\"" (I.name_of pkg);
  o "";
  List.iter (fun c ->
      print o "value ml_witness_%s(value unit) { return (((intnat)&goo_%s_witness)|1); }"
        (C.class_name c) (C.class_name c)
    ) (I.package_classes pkg);
  o "";
  List.iter (fun e ->
      print o "value Val_%s(%s v)" (I.name_of e) (C.enum_name e);
      o "{";
      o "  switch (v)";
      o "  {";
      List.iter (fun member ->
          let member = I.name_of member in
          print o "  case %s: return Val_long(%d);" member (hash_variant member);
        ) (I.enum_members e);
      o "  default:";
      o "    abort();";
      o "  }";
      o "}";
      o "";
      print o "%s %s_val(value e)" (C.enum_name e) (I.name_of e);
      o "{";
      o "  switch (Long_val(e))";
      o "  {";
      List.iter (fun member ->
          let member = I.name_of member in
          print o "  case %d: return %s;" (hash_variant member) member;
        ) (I.enum_members e);
      o "  default:";
      o "    abort();";
      o "  }";
      o "}";
      o "";
    ) (I.package_enums pkg);
  let print_func func =
    let args = List.mapi (fun i (_name, typ) ->
        match typ with
        | Bool -> sprint "Bool_val(arg%d)" i
        | Int -> sprint "Long_val(arg%d)" i
        | Float -> sprint "Double_val(arg%d)" i
        | String -> sprint "Goo_string_val(arg%d)" i
        | Flag e -> sprint "%s_val(arg%d)" (I.name_of e) i
        | Object cl ->
          sprint "$Goo_val(arg%d, %s)" i (C.class_name cl)
        | Object_option cl ->
          sprint "$Goo_val_option(arg%d, %s)" i (C.class_name cl)
        | Custom _ -> raise Not_an_ml_type
      ) (I.func_args func)
    in
    if List.exists (function (Custom _) -> true | _ -> false) (I.func_ret func) then
      raise Not_an_ml_type;
    o "";
    let argc = List.length args in
    print o "value ml_%s(%s)" (C.func_symbol func) (n_args argc);
    o "{";
    caml_param o argc;
    o "  GOO_ENTER_REGION;";
    let call args = sprint "%s(%s)" (C.func_symbol func) (String.concat ", " args) in
    let inj_typ = function
      | Bool -> "Val_bool"
      | Int -> "Val_long"
      | Float -> "caml_copy_double"
      | String -> "Val_goo_string"
      | Flag e -> sprint "Val_%s" (I.name_of e)
      | Object _ -> "$Val_goo"
      | Object_option _ -> "$Val_goo_option"
      | Custom _ -> assert false
    in
    begin match I.func_ret func with
      | [] ->
        print o "  %s;" (call args);
        o "  GOO_LEAVE_REGION;";
        o "  CAMLreturn(Val_unit);"
      | [typ] ->
        print o "  value goo_result = %s(%s);" (inj_typ typ) (call args);
        o "  GOO_LEAVE_REGION;";
        o "  CAMLreturn(goo_result);";
      | typs ->
        let args' = List.mapi (fun i typ -> let name = "ret" ^ string_of_int i  in print o "  %s;" (Goo_c.ctype name typ); (name, typ)) typs in
        print o " %s;" (call (args @ List.map (fun (x,_) -> "&" ^ x) args'));
        o "  value *tuple = goo_region_alloc();";
        print o "*tuple = caml_alloc_tuple(%d);" (List.length typs);
        List.iteri (fun i (arg,typ) -> print o " Field(*tuple,%d) = %s(%s);" i (inj_typ typ) arg) args';
        o "  value goo_result = *tuple;";
        o "  GOO_LEAVE_REGION;";
        o "  CAMLreturn(goo_result);";
    end;
    o "}";
    if (argc > 5) then bytecode_proxy o (C.func_symbol func) argc
  in
  List.iter (fun x -> try print_func x with Not_an_ml_type -> ()) (I.package_funcs pkg);
  List.iter (fun c ->
      let cname = C.class_name c in
      List.iter (fun x -> try print_func x with Not_an_ml_type -> ()) (I.class_funcs c);
      List.iter (fun event ->
          o "";
          let index = C.property_index c (I.name_of event) in
          let cargs = ("self", Object c) :: I.event_args event in
          print o "goo_bool event_%s_%s(%s)"
            (C.class_name c) (I.name_of event) (C.params_str cargs);
          o "{";
          o "  CAMLparam0();";
          print o "  CAMLlocalN(var, %d);" (1 + List.length cargs);
          print o "  var[0] = $Val_goo_handler_helper(self, %d);" index;
          o "  if (var[0] == Val_unit)";
          o "    CAMLreturn(0);";
          List.iteri (fun i (name, typ) ->
              let inj = match typ with
                | Bool -> "Val_bool"
                | Int -> "Val_long"
                | Float -> "caml_copy_double"
                | String -> "Val_goo_string"
                | Flag e -> sprint "Val_%s" (enum_name pkg e)
                | Object _ -> "$Val_goo"
                | Object_option _ -> "$Val_goo_option"
                | Custom _ -> assert false
              in
              print o "  var[%d] = %s(%s);" (i + 1) inj name;
            ) cargs;
          print o "  CAMLreturn(Bool_val(caml_callbackN(Field(var[0],%d+2),%d,&var[1])));"
            index (List.length cargs);
          o "}";
        ) (I.class_events c);
      List.iter (function
          | I.Rel_collection col ->
            let pt = I.collection_port col in
            print o "GOO_STUB_COLLECTION(%s, %s, %s, %s);"
              cname (I.name_of col) (C.class_name (I.port_target pt)) (I.name_of pt)
          | I.Rel_slot sl ->
            let pt = I.slot_port sl in
            print o "GOO_STUB_SLOT(%s, %s, %s, %s);"
              cname (I.name_of sl) (C.class_name (I.port_target pt)) (I.name_of pt)
          | I.Rel_port pt ->
            print o "GOO_STUB_PORT(%s, %s, %s);"
              cname (I.name_of pt) (C.class_name (I.port_source pt));
        ) (I.class_relations c);
    ) (I.package_classes pkg)

let generate pkg ~dir =
  let rec mkdir path =
    if Sys.file_exists path then () else
      let dir, name = Filename.dirname path, Filename.basename path in
      if dir = path then () else mkdir dir;
      (try Unix.mkdir name 0o777 with _ -> ())
  in
  mkdir dir;
  let filename base ext = Filename.concat dir (base ^ "." ^ ext) in
  Goo_c.with_file (filename (I.name_of pkg) "ml") ~force:true (print_ml_stubs pkg);
  Goo_c.with_file (filename (I.name_of pkg ^ "_stubs") "c") ~force:true (print_ml_c_stubs pkg);
  ()
