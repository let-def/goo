#include "libui_tab.h"
#define WIDGET uiTab($field(self, control))

$method libui_tab *self_new(void)
{
  libui_tab *self = $alloc();
  $field(self, control) = uiControl(uiNewTab());
  return self;
}

$method void self_set_tab_margined(libui_tab *self, int page, goo_bool margined)
{
  uiTabSetMargined(WIDGET, page, margined);
}

$method goo_bool self_is_tab_margined(libui_tab *self, int page)
{
  return uiTabMargined(WIDGET, page);
}

$method int self_num_pages(libui_tab *self)
{
  return uiTabNumPages(WIDGET);
}

$method void self_insert_at(libui_tab *self, goo_string name, int before, libui_control *child)
{
  libui_control *after = NULL;
  if (before > 0)
  {
    after = libui_tab_tabs_first(self);
    for (int i = 1; i < before && after; ++i)
      after = libui_tab_tabs_next(after);
  }
  $static(connect,tabs)(self, child, after);
  uiTabInsertAt(WIDGET, goo_string_data(name), before, $field(child, control));
}

$method void self_append(libui_tab *self, goo_string name, libui_control *child)
{
  $static(connect,tabs)(self, child, libui_tab_tabs_last(self));
  uiTabAppend(WIDGET, goo_string_data(name), $field(child, control));
}
