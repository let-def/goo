#include "libui.h"

goo_string libui_init(void)
{
  uiInitOptions o;
  o.Size = sizeof(o);
  const char *result = uiInit(&o);
  if (result)
  {
    goo_string r = goo_string_from_c(strdup(result), 1);
    uiFreeInitError(result);
    return r;
  }
  else
    return null_string;
}

void libui_uninit(void)
{
  uiUninit();
}

void libui_main(void)
{
  uiMain();
}

void libui_main_steps(void)
{
  uiMainSteps();
}

int libui_main_step(int wait)
{
  return uiMainStep(wait);
}

void libui_quit(void)
{
  uiQuit();
}

goo_string libui_open_file(libui_window *parent)
{
  char *fname = uiOpenFile(uiWindow($field(parent, control)));
  goo_string result = null_string;

  if (fname)
  {
    result = goo_string_from_c(strdup(fname), 1);
    uiFreeText(fname);
  }
  return result;
}

goo_string libui_save_file(libui_window *parent)
{
  char *fname = uiSaveFile(uiWindow($field(parent, control)));
  goo_string result = null_string;

  if (fname)
  {
    result = goo_string_from_c(strdup(fname), 1);
    uiFreeText(fname);
  }
  return result;
}

void libui_msg_box(libui_window *parent, goo_string title, goo_string description)
{
  uiMsgBox(uiWindow($field(parent, control)), goo_string_data(title), goo_string_data(description));
}

void libui_msg_box_error(libui_window *parent, goo_string title, goo_string description)
{
  uiMsgBoxError(uiWindow($field(parent, control)), goo_string_data(title), goo_string_data(description));
}
