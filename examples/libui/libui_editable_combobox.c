#include "libui_editable_combobox.h"
#define WIDGET uiEditableCombobox($field(self, control))

static void on_changed(uiEditableCombobox *e, void *self)
{
  $static(event, changed)((libui_editable_combobox*)self);
}

$method libui_editable_combobox *self_new(void)
{
  libui_editable_combobox *self = $alloc();
  $field(self, control) = uiControl(uiNewEditableCombobox());
  uiEditableComboboxOnChanged(WIDGET, on_changed, self);
  return self;
}

$method void self_set_text(libui_editable_combobox *self, goo_string text)
{
  uiEditableComboboxSetText(WIDGET, goo_string_data(text));
}

$method goo_string self_text(libui_editable_combobox *self)
{
  return goo_string_from_c(uiEditableComboboxText(WIDGET));
}

$method void self_append(libui_editable_combobox *self, goo_string text)
{
  uiEditableComboboxAppend(WIDGET, goo_string_data(text));
}
