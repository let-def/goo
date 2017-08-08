open Goo_model
module I = Introspect

let sprint = Printf.sprintf
let print o fmt = Printf.ksprintf o fmt

let c_class_name cl =
    sprint "%s_%s" (I.package_name (I.class_package cl)) (I.class_name cl)

let class_name pkg cl =
  let cl_pkg = I.class_package cl in
  if compare cl_pkg pkg = 0 || compare cl_pkg goo = 0 then
    match I.class_name cl with
    | "object" -> "goo_object"
    | str -> str
  else
    sprint "%s.%s" (String.capitalize_ascii (I.package_name cl_pkg)) (I.class_name cl)

let enum_name en = I.(mangle (enum_package en) (enum_name en))
let c_method_name cl name = c_class_name cl ^ "_" ^ name
let func_name pkg fn = I.(mangle pkg fn.I.fn_name)

exception Not_an_ml_type

let mltype pkg = function
  | Bool   -> "bool"
  | Int    -> "int"
  | Float  -> "float"
  | String -> "string"
  | Enum e -> enum_name e
  | Cobject (t, false) -> sprint "[> %s] goo" (class_name pkg t)
  | Cobject (t, true) -> sprint "[> %s] goo option" (class_name pkg t)
  | Custom _  -> raise Not_an_ml_type

let rec has_events c =
  List.exists (function (Event _) -> true | _ -> false) (I.class_fields c) ||
  match I.class_extend c with
  | None -> false
  | Some c' -> has_events c'

let ml_function pkg ?(has_events=false) args ret =
  let mltype_opt = function
    | None -> "unit"
    | Some (Cobject (t, opt)) ->
      let opt = if opt then " option" else "" in
      if has_events then
        sprint "[ %s | %s_events handler ] goo%s" (class_name pkg t) (class_name pkg t) opt
      else
        sprint "%s goo%s" (class_name pkg t) opt
    | Some x -> mltype pkg x
  in
  let rec aux acc = function
    | [] -> List.rev (mltype_opt ret :: acc)
    | (_, typ) :: xs -> aux (mltype pkg typ :: acc) xs
  in
  let args = match aux [] args with
    | [ret] -> ["unit"; ret]
    | args -> args
  in
  String.concat " -> " args

let print_ml_stubs pkg o =
  o "open Goo";
  o "";
  List.iter (fun e ->
      print o "type %s =" (enum_name e);
      match I.enum_members e with
      | [] -> assert false
      | x :: xs ->
        print o "  [ `%s" x;
        List.iter (print o "  | `%s") xs;
        o "]";
    ) (I.package_enums pkg);
  List.iter (fun c ->
      let events = List.fold_left (fun acc -> function
          | Event (name, []) -> ("  | `" ^ name) :: acc
          | Event (name, args) ->
            let args = List.map snd args in
            let args = String.concat " * " (List.map (mltype pkg) args) in
            sprint "  | `%s of (%s)\n" name args :: acc
          | _ -> acc
        ) [] (I.class_fields c)
      in
      begin match I.class_extend c with
        | None ->
          let cname = class_name pkg c in
          print o "type %s = [`%s]" (I.class_name c) cname;
          if events <> [] then (
            print o "type %s_events = [" cname;
            List.iter o events;
            o "]";
          )
        | Some c' ->
          let cname = class_name pkg c in
          let c'name = class_name pkg c' in
          print o "type %s = [`%s | %s]" cname (c_class_name c) c'name;
          if events = [] then (
            if has_events c' then
              print o "type %s_events = %s_events" (I.class_name c) c'name
          ) else (
            print o "type %s_events = [" cname;
            List.iter o events;
            if has_events c' then
              print o "  | %s_events" c'name;
            o "]";
          )
      end;
      o ""
    ) (I.package_classes pkg);
  List.iter (fun func ->
      print o "external %s : %s = \"ml_%s\""
        func.I.fn_name
        (ml_function pkg func.I.fn_args func.I.fn_ret) (func_name pkg func)
    ) (I.package_funcs pkg);
  List.iter (fun c ->
      o "";
      let stub_name fmt =
        Printf.ksprintf (fun name arity ->
            if arity > 5 then
              sprint "\"ml_bc_%s\" \"ml_%s\"" name name
            else
              sprint "\"ml_%s\"" name
          ) fmt
      in
      let cname' = I.class_name c in
      let cname = c_class_name c in
      print o "external %s_witness : unit -> %s witness = \"ml_witness_%s\"" cname' cname' cname;
      let self = ("", cobject c) in
      let f = function
          | Method (name, args, ret, _, _(*false*)) ->
            print o "external %s_%s : %s = %s"
              cname' name (ml_function pkg (self :: args) ret)
              (stub_name "%s_%s" cname name (List.length args + 1))
          | Variable (name, (Cobject _ as typ)) ->
            print o "external %s_set_%s : %s = \"ml_%s_set_%s\""
              cname' name (ml_function pkg [self; "val", typ] None) cname name;
            print o "external %s_unset_%s : %s = \"ml_%s_unset_%s\""
              cname' name (ml_function pkg [self] None) cname name
          | Constructor (name, args, _) ->
            print o "external %s_%s : %s = %s"
              cname' name (ml_function pkg ~has_events:(has_events c) args (Some (cobject c)))
              (stub_name "%s_%s" cname name (List.length args))
          | Collection (name, p) ->
            let prefix' = sprint "%s_%s" cname' name in
            let prefix = sprint "%s_%s" cname name in
            let target = class_name pkg (I.port_target p) in
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
          | Slot (name, p) ->
            let prefix' = sprint "%s_%s" cname' name in
            let prefix = sprint "%s_%s" cname name in
            let target = class_name pkg (I.port_target p) in
            print o "external %s_get : [> %s] goo -> %s goo option = \"ml_%s_get\""
              prefix' cname' target prefix;
          | Port (name, p) ->
            let prefix' = sprint "%s_%s" cname' name in
            let prefix = sprint "%s_%s" cname name in
            print o "external %s_get : [> %s] goo -> %s goo option  = \"ml_%s_get\""
              prefix' cname' (class_name pkg (I.port_source p)) prefix;
            print o "external %s_detach : [> %s] goo -> unit  = \"ml_%s_detach\""
              prefix' cname' prefix;
          | _ -> ()
      in
      List.iter (fun x -> try f x with Not_an_ml_type -> ()) (I.class_fields c)
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
  o "#include <caml/memory.h>";
  o "#include <caml/callback.h>";
  o "#include \"ml_goo.h\"";
  print o "#include \"%s.h\"" (I.package_name pkg);
  o "";
  List.iter (fun c ->
      print o "value ml_witness_%s(value unit) { return (((intnat)&goo_%s_witness)|1); }"
        (c_class_name c) (c_class_name c)
    ) (I.package_classes pkg);
  o "";
  List.iter (fun e ->
      let ename = enum_name e in
      print o "value Val_%s(%s v)" ename ename;
      o "{";
      o "  switch (v)";
      o "  {";
      List.iter (fun member ->
          print o "  case %s: return Val_long(%d);" member (hash_variant member);
        ) (I.enum_members e);
      o "  default:";
      o "    abort();";
      o "  }";
      o "}";
      o "";
      print o "%s %s_val(value e)" ename ename;
      o "{";
      o "  switch (Long_val(e))";
      o "  {";
      List.iter (fun member ->
          print o "  case %d: return %s;" (hash_variant member) member;
        ) (I.enum_members e);
      o "  default:";
      o "    abort();";
      o "  }";
      o "}";
      o "";
    ) (I.package_enums pkg);
  List.iter (fun func ->
      let args = List.mapi (fun i (_name, typ) ->
          match typ with
          | Bool -> sprint "Bool_val(arg%d)" i
          | Int -> sprint "Long_val(arg%d)" i
          | Float -> sprint "Double_val(arg%d)" i
          | String -> sprint "Goo_string_val(arg%d)" i
          | Enum e -> sprint "%s_val(arg%d)" (enum_name e) i
          | Cobject (cl, opt) ->
            let opt = if opt then "_option" else "" in
            sprint "$Goo_val%s(arg%d, %s)" opt i (c_class_name cl)
          | Custom _ -> raise Not_an_ml_type
        ) func.I.fn_args
      in
      begin match func.I.fn_ret with
        | Some (Custom _) -> raise Not_an_ml_type
        | _ -> ()
      end;
      o "";
      let argc = List.length args in
      print o "value ml_%s(%s)" (func_name pkg func) (n_args argc);
      o "{";
      caml_param o argc;
      o "  GOO_ENTER_REGION;";
      let args = String.concat ", " args in
      let call = sprint "%s(%s)" (func_name pkg func) args in
      begin match func.I.fn_ret with
        | Some typ ->
          let inj = match typ with
            | Bool -> "Val_bool"
            | Int -> "Val_long"
            | Float -> "caml_copy_double"
            | String -> "Val_goo_string"
            | Enum e -> sprint "Val_%s" (enum_name e)
            | Cobject _ -> "$Val_goo"
            | Custom _ -> assert false
          in
          print o "  value goo_result = %s(%s);" inj call;
          o "  GOO_LEAVE_REGION;";
          o "  CAMLreturn(goo_result);";
        | None ->
          print o "  %s;" call;
          o "  GOO_LEAVE_REGION;";
          o "  CAMLreturn(Val_unit);"
      end;
      o "}";
      if (argc > 5) then bytecode_proxy o (func_name pkg func) argc
    ) (I.package_funcs pkg);
  List.iter (fun c ->
      let cname = c_class_name c in
      let f = function
          | Method (name, args, ret, _, static) ->
            let args = ("", cobject c) :: args in
            let args = List.mapi (fun i (_name, typ) ->
                match typ with
                | Bool -> sprint "Bool_val(arg%d)" i
                | Int -> sprint "Long_val(arg%d)" i
                | Float -> sprint "Double_val(arg%d)" i
                | String -> sprint "Goo_string_val(arg%d)" i
                | Enum e -> sprint "%s_val(arg%d)" (enum_name e) i
                | Cobject (cl, opt) ->
                  let opt = if opt then "_option" else "" in
                  sprint "$Goo_val%s(arg%d, %s)" opt i (c_class_name cl)
                | Custom _ -> raise Not_an_ml_type
              ) args
            in
            begin match ret with
              | Some (Custom _) -> raise Not_an_ml_type
              | _ -> ()
            end;
            o "";
            let argc = List.length args in
            print o "value ml_%s_%s(%s)" cname name (n_args argc);
            o "{";
            caml_param o argc;
            o "  GOO_ENTER_REGION;";
            let args = String.concat ", " args in
            let call =
              if static then
                sprint "$static(%s, %s)(%s)" cname name args
              else
                sprint "$send($Goo_val(arg0, %s), %s)(%s)" cname name args
            in
            begin match ret with
              | Some typ ->
                let inj = match typ with
                  | Bool -> "Val_bool"
                  | Int -> "Val_long"
                  | Float -> "caml_copy_double"
                  | String -> "Val_goo_string"
                  | Enum e -> sprint "Val_%s" (enum_name e)
                  | Cobject _ -> "$Val_goo"
                  | Custom _ -> assert false
                in
                print o "  value goo_result = %s(%s);" inj call;
                o "  GOO_LEAVE_REGION;";
                o "  CAMLreturn(goo_result);";
              | None ->
                print o "  %s;" call;
                o "  GOO_LEAVE_REGION;";
                o "  CAMLreturn(Val_unit);"
            end;
            o "}";
            if (argc > 5) then bytecode_proxy o (c_method_name c name) argc
          (*| Variable (name, Cobject (cclass, opt)) ->
            o "";
            print o "value ml_%s_set_%s(value self, value arg)" cname name;
            o "{";
            o "  CAMLparam2(self, arg);";
            o "  GOO_ENTER_REGION;";
            print o "  %s *obj = $Goo_val(self, %s);" cname cname;
            print o "  $send(obj,set_%s)(obj, $Goo_val(arg, %s));" name (c_class_name cclass);
            o "  GOO_LEAVE_REGION;";
            print o "  CAMLreturn(Val_unit);";
            o "}";
            o "";
            print o "value ml_%s_unset_%s(value self)" cname name;
            o "{";
            o "  CAMLparam1(self);";
            o "  GOO_ENTER_REGION;";
            print o "  %s *obj = $Goo_val(self, %s);" cname cname;
            print o "  $send(obj,set_%s)(obj, NULL);" name;
            o "  GOO_LEAVE_REGION;";
            print o "  CAMLreturn(Val_unit);";
            o "}";*)
          | Constructor (name, args, _) ->
            o "";
            let args = List.mapi (fun i (_name, typ) ->
                match typ with
                | Bool -> sprint "Bool_val(arg%d)" i
                | Int -> sprint "Long_val(arg%d)" i
                | Float -> sprint "Double_val(arg%d)" i
                | String -> sprint "Goo_string_val(arg%d)" i
                | Enum e -> sprint "%s_val(arg%d)" (enum_name e) i
                | Cobject (cl, opt) ->
                  let opt = if opt then "_option" else "" in
                  sprint "$Goo_val%s(arg%d, %s)" opt i (c_class_name cl)
                | Custom _ -> raise Not_an_ml_type
              ) args
            in
            print o "value ml_%s_%s(%s)" cname name (n_args (List.length args));
            o "{";
            caml_param o (List.length args);
            o "  GOO_ENTER_REGION;";
            let args = String.concat ", " args in
            let call = sprint "%s_%s(%s)" cname name args in
            print o "  value goo_result = $Val_goo(%s);" call;
            o "  GOO_LEAVE_REGION;";
            print o "  CAMLreturn(goo_result);";
            o "}";
          | Event (name, args) ->
            o "";
            let cargs = ("self", cobject c) :: args in
            let cargs = List.map (fun (n,t) -> Goo_c.ctype n t) cargs in
            print o "goo_bool %s_on_%s(%s)" cname name (String.concat ", " cargs);
            o "{";
            o "  CAMLparam0();";
            caml_local o (if args = [] then 1 else 3);
            o "  var0 = $Val_goo_handler_helper(self);";
            o "  if (var0 == Val_unit)";
            o "    CAMLreturn(0);";
            if args = [] then (
              print o "  CAMLreturn(Bool_val(caml_callback2(Field(var0,1),var0,Val_long(%d))));" (hash_variant name);
            ) else (
              print o "  var1 = caml_alloc_tuple(%d);" (List.length args);
              List.iteri (fun i (name, typ) ->
                  let inj = match typ with
                    | Bool -> "Val_bool"
                    | Int -> "Val_long"
                    | Float -> "caml_copy_double"
                    | String -> "Val_goo_string"
                    | Enum e -> sprint "Val_%s" (enum_name e)
                    | Cobject _ -> "$Val_goo"
                    | Custom _ -> assert false
                  in
                  print o "  Store_field(var1, %d, %s(%s));" i inj name;
                ) args;
              o "  var2 = caml_alloc_tuple(2);";
              print o "  Field(var2, 0) = Val_long(%d);" (hash_variant name);
              o "  Field(var2, 1) = var1;";
              o "  CAMLreturn(Bool_val(caml_callback2(Field(var0,1),var0,var2)));";
            );
            o "}";
          | _ -> ()
      in
      List.iter (fun x -> try f x with Not_an_ml_type -> ()) (I.class_fields c);
      List.iter (function
          | Collection (name, p) ->
            print o "GOO_STUB_COLLECTION(%s, %s, %s, %s);"
              cname name (c_class_name (I.port_target p)) (I.port_name p)
          | Slot (name, p) ->
            print o "GOO_STUB_SLOT(%s, %s, %s, %s);"
              cname name (c_class_name (I.port_target p)) (I.port_name p)
          | Port (name, p) ->
            print o "GOO_INTERNAL_PORT(%s, %s, %s);"
              cname name (c_class_name (I.port_source p));
            print o "GOO_STUB_PORT(%s, %s, %s);"
              cname name (c_class_name (I.port_source p));
          | _ -> ()
        ) (I.class_fields c);
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
  Goo_c.with_file (filename (I.package_name pkg) "ml") ~force:true (print_ml_stubs pkg);
  Goo_c.with_file (filename (I.package_name pkg ^ "_stubs") "c") ~force:true (print_ml_c_stubs pkg);
  ()
