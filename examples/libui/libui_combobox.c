#include "libui_combobox.h"
#define WIDGET uiCombobox($field(self, control))

static void on_selected(uiCombobox *e, void *self)
{
  $static(event, selected)((libui_combobox*)self);
}

$method libui_combobox *self_new(void)
{
  libui_combobox *self = $alloc();
  $field(self, control) = uiControl(uiNewCombobox());
  uiComboboxOnSelected(WIDGET, on_selected, self);
  return self;
}

$method void self_set_selected(libui_combobox *self, int selected)
{
  uiComboboxSetSelected(WIDGET, selected);
}

$method int self_selected(libui_combobox *self)
{
  return uiComboboxSelected(WIDGET);
}

$method void self_append(libui_combobox *self, goo_string text)
{
  uiComboboxAppend(WIDGET, goo_string_data(text));
}
