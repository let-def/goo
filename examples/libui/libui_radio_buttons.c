#include "libui_radio_buttons.h"
#define WIDGET uiRadioButtons($field(self, control))

static void on_selected(uiRadioButtons *e, void *self)
{
  $static(self, on_selected)((libui_radio_buttons*)self);
}

libui_radio_buttons *libui_radio_buttons_new(void)
{
  libui_radio_buttons *self = $alloc();
  $field(self, control) = uiControl(uiNewRadioButtons());
  uiRadioButtonsOnSelected(WIDGET, on_selected, self);
  return self;
}

void libui_radio_buttons_set_selected(libui_radio_buttons *self, int selected)
{
  uiRadioButtonsSetSelected(WIDGET, selected);
}

int libui_radio_buttons_selected(libui_radio_buttons *self)
{
  return uiRadioButtonsSelected(WIDGET);
}

void libui_radio_buttons_append(libui_radio_buttons *self, goo_string text)
{
  uiRadioButtonsAppend(WIDGET, goo_string_data(text));
}
