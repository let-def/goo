#include "libui_label.h"
#define WIDGET uiLabel($field(self, control))

libui_label *libui_label_new(goo_string text)
{
  libui_label *self = $alloc();
  $field(self, control) = uiControl(uiNewLabel(goo_string_data(text)));
  return self;
}

void libui_label_set_text(libui_label *self, goo_string text)
{
  uiLabelSetText(WIDGET, goo_string_data(text));
}

goo_string libui_label_text(libui_label *self)
{
  return goo_string_from_c(uiLabelText(WIDGET), 0);
}
