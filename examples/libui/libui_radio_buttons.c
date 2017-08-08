#include "libui_radio_buttons.h"
#define WIDGET uiRadioButtons($field(self, control))

static void on_selected(uiRadioButtons *e, void *self)
{
  $static(event, selected)((libui_radio_buttons*)self);
}

$method libui_radio_buttons *self_new(void)
{
  libui_radio_buttons *self = $alloc();
  $field(self, control) = uiControl(uiNewRadioButtons());
  uiRadioButtonsOnSelected(WIDGET, on_selected, self);
  return self;
}

$method void self_set_selected(libui_radio_buttons *self, int selected)
{
  uiRadioButtonsSetSelected(WIDGET, selected);
}

$method int self_selected(libui_radio_buttons *self)
{
  return uiRadioButtonsSelected(WIDGET);
}

$method void self_append(libui_radio_buttons *self, goo_string text)
{
  uiRadioButtonsAppend(WIDGET, goo_string_data(text));
}
