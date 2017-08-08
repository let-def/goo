#include "libui_label.h"
#define WIDGET uiLabel($field(self, control))

$method libui_label *self_new(goo_string text)
{
  libui_label *self = $alloc();
  $field(self, control) = uiControl(uiNewLabel(goo_string_data(text)));
  return self;
}

$method void self_set_text(libui_label *self, goo_string text)
{
  uiLabelSetText(WIDGET, goo_string_data(text));
}

$method goo_string self_text(libui_label *self)
{
  return goo_string_from_c(uiLabelText(WIDGET));
}

/*static libui_label*
  static_self_new(goo_string text);

  static void
  static_self_set_text(libui_label *self, goo_string text);

  static goo_string
  static_self_text(libui_label *self);*/
