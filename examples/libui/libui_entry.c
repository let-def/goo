#include "libui_entry.h"
#define WIDGET uiEntry($field(self, control))

static void on_changed(uiEntry *e, void *self)
{
  $static(event, changed)((libui_entry*)self);
}

static libui_entry *init(uiEntry *entry)
{
  libui_entry *self = $alloc();
  $field(self, control) = uiControl(entry);
  uiEntryOnChanged(WIDGET, on_changed, self);
  return self;
}

$method libui_entry *self_new_search(void)
{
  return init(uiNewSearchEntry());
}

$method libui_entry *self_new_password(void)
{
  return init(uiNewPasswordEntry());
}

$method libui_entry *self_new(void)
{
  return init(uiNewEntry());
}

$method goo_bool self_is_readonly(libui_entry *self)
{
  return uiEntryReadOnly(WIDGET);
}

$method void self_set_readonly(libui_entry *self, goo_bool readonly)
{
  uiEntrySetReadOnly(WIDGET, readonly);
}

$method void self_set_text(libui_entry *self, goo_string text)
{
  uiEntrySetText(WIDGET, goo_string_data(text));
}

$method goo_string self_text(libui_entry *self)
{
  return goo_string_from_c(uiEntryText(WIDGET));
}
