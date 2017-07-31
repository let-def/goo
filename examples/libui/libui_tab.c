#include "libui_tab.h"
#define WIDGET uiTab($field(self, control))

libui_tab *libui_tab_new(void)
{
  libui_tab *self = $alloc();
  $field(self, control) = uiControl(uiNewTab());
  return self;
}

void libui_tab_set_tab_margined(libui_tab *self, int page, goo_bool margined)
{
  uiTabSetMargined(WIDGET, page, margined);
}

goo_bool libui_tab_is_tab_margined(libui_tab *self, int page)
{
  return uiTabMargined(WIDGET, page);
}

int libui_tab_num_pages(libui_tab *self)
{
  return uiTabNumPages(WIDGET);
}

void libui_tab_insert_at(libui_tab *self, goo_string name, int before, libui_control *child)
{
  libui_control *after = NULL;
  if (before > 0)
  {
    after = libui_tab_tabs_first(self);
    for (int i = 1; i < before && after; ++i)
      after = libui_tab_tabs_next(after);
  }
  _connect_tabs(self, child, after);
  uiTabInsertAt(WIDGET, goo_string_data(name), before, $field(child, control));
}

void libui_tab_append(libui_tab *self, goo_string name, libui_control *child)
{
  _connect_tabs(self, child, libui_tab_tabs_last(self));
  uiTabAppend(WIDGET, goo_string_data(name), $field(child, control));
}
