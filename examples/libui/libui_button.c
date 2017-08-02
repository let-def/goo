#include "libui_button.h"
#define WIDGET uiButton($field(self, control))

static void on_clicked(uiButton *b, void *self)
{
  $static(self, on_clicked)((libui_button*)self);
}

libui_button* libui_button_new(goo_string text)
{
  libui_button *self = $alloc();
  $field(self, control) = uiControl(uiNewButton(goo_string_data(text)));
  uiButtonOnClicked(WIDGET, on_clicked, self);
  return self;
}

void libui_button_set_text(libui_button *self, goo_string text)
{
  uiButtonSetText(WIDGET, goo_string_data(text));
}

goo_string libui_button_text(libui_button *self)
{
  return goo_string_from_c(uiButtonText(WIDGET));
}
