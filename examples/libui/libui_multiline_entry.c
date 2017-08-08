#include "libui_multiline_entry.h"
#define WIDGET uiMultilineEntry($field(self, control))

static void on_changed(uiMultilineEntry *e, void *self)
{
  $static(event, changed)((libui_multiline_entry*)self);
}

$method libui_multiline_entry *self_new(goo_bool wrap)
{
  libui_multiline_entry *self = $alloc();
  $field(self, control) =
    wrap
    ? uiControl(uiNewMultilineEntry())
    : uiControl(uiNewNonWrappingMultilineEntry());
  uiMultilineEntryOnChanged(WIDGET, on_changed, (void*)self);
  return self;
}

$method goo_bool self_is_readonly(libui_multiline_entry *self)
{
  return uiMultilineEntryReadOnly(WIDGET);
}

$method void self_set_readonly(libui_multiline_entry *self, goo_bool readonly)
{
  uiMultilineEntrySetReadOnly(WIDGET, readonly);
}

$method void self_append(libui_multiline_entry *self, goo_string text)
{
  uiMultilineEntryAppend(WIDGET, goo_string_data(text));
}

$method void self_set_text(libui_multiline_entry *self, goo_string text)
{
  uiMultilineEntrySetText(WIDGET, goo_string_data(text));
}

$method goo_string self_text(libui_multiline_entry *self)
{
  return goo_string_from_c(uiMultilineEntryText(WIDGET));
}
