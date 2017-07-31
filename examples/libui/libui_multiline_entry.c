#include "libui_multiline_entry.h"
#define WIDGET uiMultilineEntry($field(self, control))

static void on_changed(uiMultilineEntry *e, void *self)
{
  $static(self, on_changed)((libui_multiline_entry*)self);
}

libui_multiline_entry * libui_multiline_entry_new(goo_bool wrap)
{
  libui_multiline_entry *self = $alloc();
  $field(self, control) =
    wrap
    ? uiControl(uiNewMultilineEntry())
    : uiControl(uiNewNonWrappingMultilineEntry());
  uiMultilineEntryOnChanged(WIDGET, on_changed, (void*)self);
  return self;
}

goo_bool  libui_multiline_entry_is_readonly(libui_multiline_entry *self)
{
  return uiMultilineEntryReadOnly(WIDGET);
}

void  libui_multiline_entry_set_readonly(libui_multiline_entry *self, goo_bool readonly)
{
  uiMultilineEntrySetReadOnly(WIDGET, readonly);
}

void  libui_multiline_entry_append(libui_multiline_entry *self, goo_string text)
{
  uiMultilineEntryAppend(WIDGET, goo_string_data(text));
}

void  libui_multiline_entry_set_text(libui_multiline_entry *self, goo_string text)
{
  uiMultilineEntrySetText(WIDGET, goo_string_data(text));
}

goo_string  libui_multiline_entry_text(libui_multiline_entry *self)
{
  return goo_string_from_c(uiMultilineEntryText(WIDGET), 0);
}
