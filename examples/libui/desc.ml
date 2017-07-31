open Goo_model

let ui = package "libui"
let static name = meth ~static:true name
let prop name = group [
    static ("is_" ^ name) [] ~ret:bool;
    static ("set_" ^ name) [arg name bool];
  ]

let () =
  declare ui ["#include \"ui.h\""];
  func ui "init" [] ~ret:string;
  func ui "uninit" [];
  func ui "main" [];
  func ui "main_steps" [];
  func ui "main_step" [arg "wait" int] ~ret:int;
  func ui "quit" []

let control = cclass ui "control" [
    override "destroy";
    variable "control" (custom "uiControl *");
    (* uintptr_t uiControlHandle(uiControl * ); *)
    static "is_toplevel" [] ~ret:bool;
    static "is_visible" [] ~ret:bool;
    static "show" [];
    static "hide" [];
    static "is_enabled" [] ~ret:bool;
    static "enable" [];
    static "disable" [];
  ]

let control_parent = port control "parent" control

let window = cclass ui "window" ~extend:control [
    static "title" [] ~ret:string;
    static "set_title" [arg "title" string];
    static "content_width" [] ~ret:int;
    static "content_height" [] ~ret:int;
    static "set_content_size" [arg "width" int; arg "height" int];
    prop "fullscreen";
    event "content_size_changed" [];
    event "closing" [];
    prop "borderless";
    slot "child" control_parent;
    meth "child_connect" [arg "val" (cobject control)];
    override "on_child_disconnect";
    prop "margined";

    constructor "new" [arg "title" string; arg "width" int; arg "height" int; arg "has_menubar" bool];
  ]

let button = cclass ui "button" ~extend:control [
    static "text" [] ~ret:string;
    static "set_text" [arg "text" string];
    event "clicked" [];
    constructor "new" [arg "text" string];
  ]

let box = cclass ui "box" ~extend:control [
    collection "children" control_parent;
    static "append" [arg "child" (cobject control); arg "stretchy" bool];
    prop "padded";
    constructor "new_horizontal" [];
    constructor "new_vertical" [];
  ]

let checkbox = cclass ui "checkbox" ~extend:control [
    static "text" [] ~ret:string;
    static "set_text" [arg "text" string];
    event "toggled" [];
    prop "checked";
    constructor "new" [arg "text" string];
  ]

let entry = cclass ui "entry" ~extend:control [
    static "text" [] ~ret:string;
    static "set_text" [arg "text" string];
    event "changed" [];
    prop "readonly";
    constructor "new" [];
    constructor "new_password" [];
    constructor "new_search" [];
  ]

let label = cclass ui "label" ~extend:control [
    static "text" [] ~ret:string;
    static "set_text" [arg "text" string];
    constructor "new" [arg "text" string];
  ]

let tab = cclass ui "tab" ~extend:control [
    static "num_pages" [] ~ret:int;
    collection "tabs" control_parent;
    static "append" [arg "name" string; arg "child" (cobject control)];
    static "insert_at" [arg "name" string; arg "before" int; arg "child" (cobject control)];
    static "is_tab_margined" [arg "page" int] ~ret:bool;
    static "set_tab_margined" [arg "page" int; arg "margined" bool];
    constructor "new" [];
  ]

let group = cclass ui "group" ~extend:control [
    static "title" [] ~ret:string;
    static "set_title" [arg "title" string];
    slot "child" control_parent;
    static "child_connect" [arg "val" (cobject control)];
    override "on_child_disconnect";
    prop "margined";
    constructor "new" [arg "title" string];
  ]

let spinbox = cclass ui "spinbox" ~extend:control [
    static "value" [] ~ret:int;
    static "set_value" [arg "value" int];
    event "changed" [];
    constructor "new" [arg "min" int; arg "max" int];
  ]

let slider = cclass ui "slider" ~extend:control [
    static "value" [] ~ret:int;
    static "set_value" [arg "value" int];
    event "changed" [];
    constructor "new" [arg "min" int; arg "max" int];
  ]

let slider = cclass ui "progressbar" ~extend:control [
    static "value" [] ~ret:int;
    static "set_value" [arg "value" int];
    constructor "new" [];
  ]

let separator = cclass ui "separator" ~extend:control [
    constructor "new_horizontal" [];
    constructor "new_vertical" [];
  ]

let combobox = cclass ui "combobox" ~extend:control [
    static "append" [arg "text" string];
    static "selected" [] ~ret:int;
    static "set_selected" [arg "selected" int];
    event "selected" [];
    constructor "new" [];
  ]

let editable_combobox = cclass ui "editable_combobox" ~extend:control [
    static "append" [arg "text" string];
    static "text" [] ~ret:string;
    static "set_text" [arg "text" string];
    event "changed" [];
    constructor "new" [];
  ]

let radio_buttons = cclass ui "radio_buttons" ~extend:control [
    static "append" [arg "text" string];
    static "selected" [] ~ret:int;
    static "set_selected" [arg "selected" int];
    event "selected" [];
    constructor "new" [];
  ]

let date_time_picker = cclass ui "date_time_picker" ~extend:control [
    constructor "new" [];
    constructor "new_date" [];
    constructor "new_time" [];
  ]

let multiline_entry = cclass ui "multiline_entry" ~extend:control [
    static "text" [] ~ret:string;
    static "set_text" [arg "text" string];
    static "append" [arg "text" string];
    event "changed" [];
    prop "readonly";
    constructor "new" [arg "wrap" bool];
  ]

let menu = cclass ui "menu" []

let menu_item = cclass ui "menu_item" [
    variable "control" (custom "uiMenuItem *");
    static "enable" [];
    static "disable" [];
    event "clicked" [];
    prop "checked";
    constructor "new" [arg "item" (custom "uiMenuItem *")];
  ]

let menu_item_parent = port menu_item "parent" menu

let () = fields menu [
    variable "control" (custom "uiMenu *");
    collection "items" menu_item_parent;
    static "append_item" [arg "name" string] ~ret:(cobject menu_item);
    static "append_check_item" [arg "name" string] ~ret:(cobject menu_item);
    static "append_quit_item" [] ~ret:(cobject menu_item);
    static "append_preferences_item" [] ~ret:(cobject menu_item);
    static "append_about_item" [] ~ret:(cobject menu_item);
    static "append_separator" [];
    constructor "new" [arg "name" string];
  ]

let () = (
  func ui "open_file" [arg "parent" (cobject window)] ~ret:string;
  func ui "save_file" [arg "parent" (cobject window)] ~ret:string;
  func ui "msg_box" [arg "parent" (cobject window); arg "title" string; arg "description" string];
  func ui "msg_box_error" [arg "parent" (cobject window); arg "title" string; arg "description" string];
)

let font_button = cclass ui "font_button" ~extend:control [
    (*_UI_EXTERN uiDrawTextFont *uiFontButtonFont(uiFontButton *b);*)
    event "changed" [];
    constructor "new" [];
  ]

let color_button = cclass ui "color_button" ~extend:control [
    (*_UI_EXTERN void uiColorButtonColor(uiColorButton *b, double *r, double *g, double *bl, double *a);*)
    static "set_color" [arg "r" float; arg "g" float; arg "b" float; arg "a" float];
    event "changed" [];
    constructor "new" [];
  ]

let form = cclass ui "form" ~extend:control [
    collection "children" control_parent;
    static "append" [arg "name" string; arg "c" (cobject control); arg "stretchy" bool];
    static "delete" [arg "child" (cobject control)];
    prop "padded";
    constructor "new" [];
  ]

let align = enum ui "align" ["Fill"; "Start"; "Center"; "End"]
let at = enum ui "at" ["Leading"; "Top"; "Trailing"; "Bottom"]

let grid = cclass ui "grid" ~extend:control [
    collection "children" control_parent;
    static "append" [arg "c" (cobject control);
                     arg "left" int; arg "top" int;
                     arg "xspan" int; arg "yspan" int;
                     arg "hexpand" bool; arg "halign" align;
                     arg "vexpand" bool; arg "valign" align;
                    ];
    static "insert_at" [arg "c" (cobject control);
                        arg "existing" (cobject control); arg "at" at;
                        arg "xspan" int; arg "yspan" int;
                        arg "hexpand" bool; arg "halign" align;
                        arg "vexpand" bool; arg "valign" align;
                       ];
    prop "padded";
    constructor "new" [];
  ]

let () =
  Goo_c.generate ui ~dir:"./";
  Goo_ml.generate ui ~dir:"./"
