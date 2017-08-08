#include "libui_button.h"
#define WIDGET uiButton($field(self, control))

static void on_clicked(uiButton *b, void *self)
{
  $static(event, clicked)((libui_button*)self);
}

$method libui_button* self_new(goo_string text)
{
  libui_button *self = $alloc();
  $field(self, control) = uiControl(uiNewButton(goo_string_data(text)));
  uiButtonOnClicked(WIDGET, on_clicked, self);
  return self;
}

$method void self_set_text(libui_button *self, goo_string text)
{
  uiButtonSetText(WIDGET, goo_string_data(text));
}

$method goo_string self_text(libui_button *self)
{
  return goo_string_from_c(uiButtonText(WIDGET));
}
