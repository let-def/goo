#use "topfind";;
#require "goo-gen";;
open Goo_model

(* Some generic definitions to help binding libui *)

(* A self_meth is a method that takes an instance of self as the first argument.  *)
let self_meth cl ret name args =
  meth cl ret name (arg "self" (Object cl) :: args)

(* The `'` variant returns an abstract name that identifies the method:
     val self_meth : classe -> ... -> unit
     val self_meth' : classe -> ... -> func
*)
let self_meth' cl ret name args =
  meth' cl ret name (arg "self" (Object cl) :: args)

(* A constructor is a method that returns an instance but doesn't take it as
   first argument. *)
let constructor cl name args =
  Goo_c.set_concrete cl;
  meth cl [Object cl] name args

(* A (boolean) property is a pair of methods: one to set, one to get. *)
let prop cl name =
  self_meth cl [bool] ("is_" ^ name) [];
  self_meth cl [] ("set_" ^ name) [arg name bool]

(* -- Declaration begins here.
   `val ui : package` will be the abstract name that identifies the libui package.
*)
let ui = package "libui"

(* Some toplevel function definitions *)
let () =
  (* The C compiler needs to know about the libui headers.
     However this is a specific knowledge that the ML interface generator
     doesn't need to know about.
     `Goo_model` is about declaring the shared parts.  Entities of the model
     are given abstract name.  Specific knowledge can then be added
     per-backend. *)
  Goo_c.package_declare ui ["#include \"ui.h\""];
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
  func ui [string] "init" [];
  func ui [] "uninit" [];
  func ui [] "main" [];
  func ui [] "main_steps" [];
  func ui [int] "main_step" [arg "wait" int];
  func ui [] "quit" []

(* The root of libui class hierarchy. *)
let control = classe ui "control"

let () =
  (* All classe inherits from "goo_object", which is part of the runtime
     support library.
     The only method declared by "goo_object" is destroy. This is a dynamic
     method (dispatched based on the actual class of its first argument) that
     can be given a more specific definition by sub-classes.
     But dynamicity and redefinition are implementation details and don't
     affect the interface. Hence, override only matters to C backend.
  *)
  Goo_c.override control goo_destroy;
  (* A control object is just a wrapper around uiControl: each instance
     contains a control field that points to the actual control.
     Like dynamic dispatch information, instance variables only matter to the
     implementation. They are not exposed in the interface.
  *)
  Goo_c.instance_variable control "control" (Custom "uiControl *");
  (* uintptr_t uiControlHandle(uiControl * ); *)
  (* Other basic methods. *)
  self_meth control [bool] "is_toplevel" [];
  self_meth control [bool] "is_visible" [];
  self_meth control [] "show" [];
  self_meth control [] "hide" [];
  self_meth control [bool] "is_enabled" [];
  self_meth control [] "enable" [];
  self_meth control [] "disable" []

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

   A last case of graph structure is captured: when one declares an
   instance_variable of type (Object ...), the GC will be notified. However
   this won't appear in the interface.

   Together, the "symmetric" port, slot and collection and the anonymous
   variable allow to capture all graph shapes, while ensuring safety and
   friendly programming style (wellformed-ness of the graph is ensured).
   The cost is that these relations must be declared upfront.
   *)
let control_parent = port control "parent" control

let window = classe ui "window" ~extend:control

let () =
  self_meth window [string] "title" [];
  self_meth window [] "set_title" [arg "title" string];
  (*self_meth window [int; int] "content_size" [];*)
  self_meth window [] "set_content_size" [arg "width" int; arg "height" int];
  prop window "fullscreen";
  prop window "borderless";
  (* Events.
     Events allow control to call back to the interface language.
     Each event is an optional closure that can be set from ML. *)
  event window [] "content_size_changed" [];
  event window [] "closing" [];
  (* The C backend allows C-code to be notified when a slot is disconnected.
     The method "on_child_disconnect" will be invoked.  The abstract names
     returned by slot' and self_meth' are used to connect both.
  *)
  Goo_c.on_slot_disconnect
    (slot' window "child" control_parent)
    (self_meth' window [] "on_child_disconnect" ["object", Object control]);
  self_meth window [] "child_connect" [arg "val" (Object control)];
  prop window "margined";
  constructor window "new"
        [arg "title" string; arg "width" int; arg "height" int; arg "has_menubar" bool]

(* The rest of the file just builds on these concepts to bind the rest of the API.
   Jump to the last lines for the end of the introduction. *)

let button = classe ui "button" ~extend:control

let () =
  self_meth button [string] "text" [];
  self_meth button [] "set_text" [arg "text" string];
  event button [] "clicked" [];
  constructor button "new" [arg "text" string]

let box = classe ui "box" ~extend:control

let () =
  collection box "children" control_parent;
  self_meth box [] "append" [arg "child" (Object control); arg "stretchy" bool];
  prop box "padded";
  constructor box "new_horizontal" [];
  constructor box "new_vertical" []

let checkbox = classe ui "checkbox" ~extend:control

let ()=
  self_meth checkbox [string] "text" [];
  self_meth checkbox [] "set_text" [arg "text" string];
  event checkbox [] "toggled" [];
  prop checkbox "checked";
  constructor checkbox "new" [arg "text" string]

let entry = classe ui "entry" ~extend:control

let () =
  self_meth entry [string] "text" [];
  self_meth entry [] "set_text" [arg "text" string];
  event entry [] "changed" [];
  prop entry "readonly";
  constructor entry "new" [];
  constructor entry "new_password" [];
  constructor entry "new_search" []

let label = classe ui "label" ~extend:control

let () =
  self_meth label [string] "text" [];
  self_meth label [] "set_text" [arg "text" string];
  constructor label "new" [arg "text" string]

let tab = classe ui "tab" ~extend:control

let () =
  self_meth tab [int] "num_pages" [];
  collection tab "tabs" control_parent;
  self_meth tab [] "append" [arg "name" string; arg "child" (Object control)];
  self_meth tab [] "insert_at" [arg "name" string; arg "before" int; arg "child" (Object control)];
  self_meth tab [bool] "is_tab_margined" [arg "page" int];
  self_meth tab [] "set_tab_margined" [arg "page" int; arg "margined" bool];
  constructor tab "new" []

let group = classe ui "group" ~extend:control

let () =
  self_meth group [string] "title" [];
  self_meth group [] "set_title" [arg "title" string];
  self_meth group [] "child_connect" [arg "val" (Object control)];
  Goo_c.on_slot_disconnect
    (slot' group "child" control_parent)
    (self_meth' group [] "on_child_disconnect" ["object", Object control]);
  prop group "margined";
  constructor group "new" [arg "title" string]

let spinbox = classe ui "spinbox" ~extend:control

let () =
  self_meth spinbox [int] "value" [];
  self_meth spinbox [] "set_value" [arg "value" int];
  event spinbox [] "changed" [];
  constructor spinbox "new" [arg "min" int; arg "max" int]

let slider = classe ui "slider" ~extend:control

let () =
  self_meth slider [int] "value" [];
  self_meth slider [] "set_value" [arg "value" int];
  event slider [] "changed" [];
  constructor slider "new" [arg "min" int; arg "max" int]

let slider = classe ui "progressbar" ~extend:control

let () =
  self_meth slider [int] "value" [];
  self_meth slider [] "set_value" [arg "value" int];
  constructor slider "new" []

let separator = classe ui "separator" ~extend:control

let () =
  constructor separator "new_horizontal" [];
  constructor separator "new_vertical" []

let combobox = classe ui "combobox" ~extend:control

let () =
  self_meth combobox [] "append" [arg "text" string];
  self_meth combobox [int] "selected" [];
  self_meth combobox [] "set_selected" [arg "selected" int];
  event combobox [] "selected" [];
  constructor combobox "new" []

let editable_combobox = classe ui "editable_combobox" ~extend:control

let () =
  self_meth editable_combobox [] "append" [arg "text" string];
  self_meth editable_combobox [string] "text" [];
  self_meth editable_combobox [] "set_text" [arg "text" string];
  event editable_combobox [] "changed" [];
  constructor editable_combobox "new" []

let radio_buttons = classe ui "radio_buttons" ~extend:control

let () =
  self_meth radio_buttons [] "append" [arg "text" string];
  self_meth radio_buttons [int] "selected" [];
  self_meth radio_buttons [] "set_selected" [arg "selected" int];
  event radio_buttons [] "selected" [];
  constructor radio_buttons "new" []

let date_time_picker = classe ui "date_time_picker" ~extend:control

let () =
  constructor date_time_picker "new" [];
  constructor date_time_picker "new_date" [];
  constructor date_time_picker "new_time" []

let multiline_entry = classe ui "multiline_entry" ~extend:control

let () =
  self_meth multiline_entry [string] "text" [];
  self_meth multiline_entry [] "set_text" [arg "text" string];
  self_meth multiline_entry [] "append" [arg "text" string];
  event multiline_entry [] "changed" [];
  prop multiline_entry "readonly";
  constructor multiline_entry "new" [arg "wrap" bool]

let menu = classe ui "menu"

let menu_item = classe ui "menu_item"

let () =
  Goo_c.instance_variable menu_item "control" (Custom "uiMenuItem *");
  self_meth menu_item [] "enable" [];
  self_meth menu_item [] "disable" [];
  event menu_item [] "clicked" [];
  prop menu_item "checked";
  constructor menu_item "new" [arg "item" (Custom "uiMenuItem *")]

let menu_item_parent = port menu_item "parent" menu

let () =
  Goo_c.instance_variable menu "control" (Custom "uiMenu *");
  collection menu "items" menu_item_parent;
  self_meth menu [Object menu_item] "append_item" [arg "name" string];
  self_meth menu [Object menu_item] "append_check_item" [arg "name" string];
  self_meth menu [Object menu_item] "append_quit_item" [];
  self_meth menu [Object menu_item] "append_preferences_item" [];
  self_meth menu [Object menu_item] "append_about_item" [];
  self_meth menu [] "append_separator" [];
  constructor menu "new" [arg "name" string]

let () =
  func ui [string] "open_file" [arg "parent" (Object window)];
  func ui [string] "save_file" [arg "parent" (Object window)];
  func ui [] "msg_box" [arg "parent" (Object window); arg "title" string; arg "description" string];
  func ui [] "msg_box_error" [arg "parent" (Object window); arg "title" string; arg "description" string]

let font_button = classe ui "font_button" ~extend:control

let () =
  (*_UI_EXTERN uiDrawTextFont *uiFontButtonFont(uiFontButton *b);*)
  event font_button [] "changed" [];
  constructor font_button "new" []

let color_button = classe ui "color_button" ~extend:control

let () =
  (*self_meth color_button [float; float; float; float] "get_color" [];*)
  self_meth color_button [] "set_color" [arg "r" float; arg "g" float; arg "b" float; arg "a" float];
  event color_button [] "changed" [];
  constructor color_button "new" []

let form = classe ui "form" ~extend:control

let () =
  collection form "children" control_parent;
  self_meth form [] "append" [arg "name" string; arg "c" (Object control); arg "stretchy" bool];
  self_meth form [] "delete" [arg "child" (Object control)];
  prop form "padded";
  constructor form "new" []

let align = enum ui "align"
let () = List.iter (enum_member align) ["Fill"; "Start"; "Center"; "End"]

let at = enum ui "at"
let () = List.iter (enum_member at) ["Leading"; "Top"; "Trailing"; "Bottom"]

let grid = classe ui "grid" ~extend:control

let () =
  collection grid "children" control_parent;
  self_meth grid [] "append" [
    arg "c" (Object control);
    arg "left" int; arg "top" int;
    arg "xspan" int; arg "yspan" int;
    arg "hexpand" bool; arg "halign" (flag align);
    arg "vexpand" bool; arg "valign" (flag align);
  ];
  self_meth grid [] "insert_at" [
    arg "c" (Object control);
    arg "existing" (Object control); arg "at" (flag at);
    arg "xspan" int; arg "yspan" int;
    arg "hexpand" bool; arg "halign" (flag align);
    arg "vexpand" bool; arg "valign" (flag align);
  ];
  prop grid "padded";
  constructor grid "new" []

(* The model and backends have now been fed with the description of library.
   We can tell the generators to start their work. *)
let () =
  (* The C generator will produce
     - libui.h for shared definitions
     - for each class, a file libui_<class>.h that contains private definitions.
     Template files libui_manual.c libui_<class>.c are generated if they don't
     already exist with the skeleton of the implementation.  In most cases this
     implementation should be filled manually.
  *)
  Goo_c.generate ui ~dir:"./";
  (* The ML generator will produce libui_stubs.c and libui.ml that binds the
     model above to OCaml runtime. Nothing has to be written manually. *)
  Goo_ml.generate ui ~dir:"./"
;;
