open Libui

let make_basic_controls_page () =
  let hbox =
    let hbox = box_new_horizontal () in
    box_set_padded hbox true;
    box_append hbox (button_new "Button") false;
    box_append hbox (checkbox_new "Checkbox") false;
    box_append hbox (label_new "This is a label. Right now, labels can only span one line.") false;
    box_append hbox (separator_new_horizontal ()) false;
    hbox
  in
  let group =
    let group = group_new "Entries" in
    group_set_margined group true;
    let entry_form = form_new () in
    form_set_padded entry_form true;
    group_child_connect group entry_form;
    form_append entry_form "Entry" (entry_new ()) false;
    form_append entry_form "Password Entry" (entry_new_password ()) false;
    form_append entry_form "Search Entry" (entry_new_search ()) false;
    form_append entry_form "Multiline Entry" (multiline_entry_new true) true;
    form_append entry_form "Multiline Entry No Wrap" (multiline_entry_new false) true;
    group
  in
  let vbox = box_new_vertical() in
  box_set_padded vbox true;
  box_append vbox hbox false;
  box_append vbox group true;
  vbox

let make_numbers_page () =
  let hbox = box_new_horizontal () in
  box_set_padded hbox true;
  let group = group_new "Numbers" in
  group_set_margined group true;
  box_append hbox group true;

  let vbox = box_new_vertical () in
  box_set_padded vbox true;
  group_child_connect group vbox;

  let spinbox = spinbox_new 0 100 in
	let slider = slider_new 0 100 in
	let pbar = progressbar_new () in
  Goo.set_handler spinbox (fun self `changed ->
      slider_set_value slider (spinbox_value self);
      progressbar_set_value pbar (spinbox_value self);
      true
    );
  Goo.set_handler slider (fun self `changed ->
      spinbox_set_value spinbox (slider_value self);
      progressbar_set_value pbar (slider_value self);
      true
    );
  box_append vbox spinbox false;
  box_append vbox slider false;
  box_append vbox pbar false;

  let ip = progressbar_new () in
  progressbar_set_value ip (-1);
  box_append vbox ip false;

  let group = group_new "Lists" in
  group_set_margined group true;
  box_append hbox group true;

  let vbox = box_new_vertical () in
  box_set_padded vbox true;
  group_child_connect group vbox;

  let cbox = combobox_new () in
  combobox_append cbox "Combobox Item 1";
  combobox_append cbox "Combobox Item 2";
  combobox_append cbox "Combobox Item 3";
  box_append vbox cbox false;

  let ecbox = editable_combobox_new () in
  editable_combobox_append ecbox "Editable Item 1";
  editable_combobox_append ecbox "Editable Item 2";
  editable_combobox_append ecbox "Editable Item 3";
  box_append vbox ecbox false;

  let rb = radio_buttons_new () in
  radio_buttons_append rb "Radio Button 1";
  radio_buttons_append rb "Radio Button 2";
  radio_buttons_append rb "Radio Button 3";
  box_append vbox rb false;
  hbox

(*static void onOpenFileClicked(uiButton *b, void *data)
  {
    uiEntry *entry = uiEntry(data);
    char *filename;

    filename = uiOpenFile(mainwin);
    if (filename == NULL) {
        uiEntrySetText(entry, "(cancelled)");
        return;
      }
        uiEntrySetText(entry, filename);
      uiFreeText(filename);
  }

  static void onSaveFileClicked(uiButton *b, void *data)
  {
    uiEntry *entry = uiEntry(data);
    char *filename;

    filename = uiSaveFile(mainwin);
    if (filename == NULL) {
        uiEntrySetText(entry, "(cancelled)");
        return;
      }
        uiEntrySetText(entry, filename);
      uiFreeText(filename);
  }

  static void onMsgBoxClicked(uiButton *b, void *data)
  {
    uiMsgBox(mainwin,
             "This is a normal message box.",
             "More detailed information can be shown here.");
  }

  static void onMsgBoxErrorClicked(uiButton *b, void *data)
  {
    uiMsgBoxError(mainwin,
                  "This message box describes an error.",
                  "More detailed information can be shown here.");
  }*)

let make_data_choosers_page mainwin =
  let hbox = box_new_horizontal () in
  box_set_padded hbox true;
  let vbox = box_new_vertical () in
  box_set_padded vbox true;
  box_append hbox vbox false;
  box_append vbox (date_time_picker_new_date ()) false;
  box_append vbox (date_time_picker_new_time ()) false;
  box_append vbox (date_time_picker_new ()) false;
  box_append vbox (font_button_new ()) false;
  box_append vbox (color_button_new ()) false;
  box_append hbox (separator_new_vertical ()) false;

  let vbox = box_new_vertical () in
  box_set_padded vbox true;
  box_append hbox vbox true;

  let grid = grid_new () in
  grid_set_padded grid true;
  box_append vbox grid false;

  let button = button_new "Open File" in
  let entry = entry_new () in
  entry_set_readonly entry true;
  Goo.set_handler button (fun self `clicked ->
      let filename = open_file mainwin in
      entry_set_text entry (if filename = "" then "(cancelled)" else filename);
      true
    );
  grid_append grid button 0 0 1 1 false `Fill false `Fill;
  grid_append grid entry  1 0 1 1 true  `Fill false `Fill;

  let button = button_new "Save File" in
  let entry = entry_new () in
  entry_set_readonly entry true;
  Goo.set_handler button (fun self `clicked ->
      let filename = save_file mainwin in
      entry_set_text entry (if filename = "" then "(cancelled)" else filename);
      true
    );
  grid_append grid button 0 1 1 1 false `Fill false `Fill;
  grid_append grid entry  1 1 1 1 true  `Fill false `Fill;

  let msggrid = grid_new () in
  grid_set_padded msggrid true;
  grid_append grid msggrid 0 2 2 1 false `Center false `Start;

  let button = button_new "Message Box" in
  Goo.set_handler button (fun self `clicked ->
      msg_box mainwin
        "This is a normal message box."
        "More detailed information can be shown here.";
      true
    );
  grid_append msggrid button 0 0 1 1 false `Fill false `Fill;
  let button = button_new "Error Box" in
  Goo.set_handler button (fun self `clicked ->
      msg_box_error mainwin
        "This message box describes an error."
        "More detailed information can be shown here.";
      true
    );
  grid_append msggrid button 1 0 1 1 false `Fill false `Fill;
  hbox
;;

(*static int onShouldQuit(void *data)
{
	uiWindow *mainwin = uiWindow(data);

	uiControlDestroy(uiControl(mainwin));
	return 1;
}*)

let main () =
  begin match init () with
    | "" -> ()
    | err -> failwith err
  end;
  let mainwin = window_new "OCaml goo/libui Control Gallery" 640 480 true in
  Goo.set_handler mainwin (fun self -> function
      | `closing -> quit (); true
      | _ -> false);
  (*uiOnShouldQuit(onShouldQuit, mainwin);*)

  let tab = tab_new () in
  window_child_connect mainwin tab;
  window_set_margined mainwin true;

  tab_append tab "Basic Controls" (make_basic_controls_page ());
  tab_set_tab_margined tab 0 true;

  tab_append tab "Numbers and Lists" (make_numbers_page ());
  tab_set_tab_margined tab 0 true;
  tab_set_tab_margined tab 1 true;

  tab_append tab "Data Choosers" (make_data_choosers_page mainwin);
  tab_set_tab_margined tab 2 true;

  control_show mainwin;
  main ()

let () = main ()
