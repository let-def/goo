#include "libui_combobox.h"
#define WIDGET uiCombobox($field(self, control))

static void on_selected(uiCombobox *e, void *self)
{
  $static(self, on_selected)((libui_combobox*)self);
}

libui_combobox *libui_combobox_new(void)
{
  libui_combobox *self = $alloc();
  $field(self, control) = uiControl(uiNewCombobox());
  uiComboboxOnSelected(WIDGET, on_selected, self);
  return self;
}

void libui_combobox_set_selected(libui_combobox *self, int selected)
{
  uiComboboxSetSelected(WIDGET, selected);
}

int libui_combobox_selected(libui_combobox *self)
{
  return uiComboboxSelected(WIDGET);
}

void libui_combobox_append(libui_combobox *self, goo_string text)
{
  uiComboboxAppend(WIDGET, goo_string_data(text));
}
