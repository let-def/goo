#include "libui_entry.h"
#define WIDGET uiEntry($field(self, control))

static void on_changed(uiEntry *e, void *self)
{
  $static(self, on_changed)((libui_entry*)self);
}

static libui_entry *init(uiEntry *entry)
{
  libui_entry *self = $alloc();
  $field(self, control) = uiControl(entry);
  uiEntryOnChanged(WIDGET, on_changed, self);
  return self;
}

libui_entry *libui_entry_new_search(void)
{
  return init(uiNewSearchEntry());
}

libui_entry *libui_entry_new_password(void)
{
  return init(uiNewPasswordEntry());
}

libui_entry *libui_entry_new(void)
{
  return init(uiNewEntry());
}

goo_bool libui_entry_is_readonly(libui_entry *self)
{
  return uiEntryReadOnly(WIDGET);
}

void libui_entry_set_readonly(libui_entry *self, goo_bool readonly)
{
  uiEntrySetReadOnly(WIDGET, readonly);
}

void libui_entry_set_text(libui_entry *self, goo_string text)
{
  uiEntrySetText(WIDGET, goo_string_data(text));
}

goo_string libui_entry_text(libui_entry *self)
{
  return goo_string_from_c(uiEntryText(WIDGET));
}
