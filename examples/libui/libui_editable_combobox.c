#include "libui_editable_combobox.h"
#define WIDGET uiEditableCombobox($field(self, control))

static void on_changed(uiEditableCombobox *e, void *self)
{
  $static(self, on_changed)((libui_editable_combobox*)self);
}

libui_editable_combobox *libui_editable_combobox_new(void)
{
  libui_editable_combobox *self = $alloc();
  $field(self, control) = uiControl(uiNewEditableCombobox());
  uiEditableComboboxOnChanged(WIDGET, on_changed, self);
  return self;
}

void libui_editable_combobox_set_text(libui_editable_combobox *self, goo_string text)
{
  uiEditableComboboxSetText(WIDGET, goo_string_data(text));
}

goo_string libui_editable_combobox_text(libui_editable_combobox *self)
{
  return goo_string_from_c(uiEditableComboboxText(WIDGET), 0);
}

void libui_editable_combobox_append(libui_editable_combobox *self, goo_string text)
{
  uiEditableComboboxAppend(WIDGET, goo_string_data(text));
}
