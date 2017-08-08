#include "libui_checkbox.h"
#define WIDGET uiCheckbox($field(self, control))

static void on_toggled(uiCheckbox *b, void *self)
{
  $static(event, toggled)((libui_checkbox*)self);
}

libui_checkbox *self_new(goo_string text)
{
  libui_checkbox *self = $alloc();
  $field(self, control) = uiControl(uiNewCheckbox(goo_string_data(text)));
  uiCheckboxOnToggled(WIDGET, on_toggled, self);
  return self;
}

goo_bool self_is_checked(libui_checkbox *self)
{
  return uiCheckboxChecked(WIDGET);
}

void self_set_checked(libui_checkbox *self, goo_bool checked)
{
  uiCheckboxSetChecked(WIDGET, checked);
}

void self_set_text(libui_checkbox *self, goo_string text)
{
  uiCheckboxSetText(WIDGET, goo_string_data(text));
}

goo_string self_text(libui_checkbox *self)
{
  return goo_string_from_c(uiCheckboxText(WIDGET));
}
