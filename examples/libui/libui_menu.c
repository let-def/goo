#include "libui_menu.h"
#define WIDGET $field(self, control)

$method libui_menu *self_new(goo_string name)
{
  libui_menu *self = $alloc();
  $field(self, control) = uiNewMenu(goo_string_data(name));
  return self;
}

static libui_menu_item *append(libui_menu *self, uiMenuItem *item)
{
  libui_menu_item *result = libui_menu_item_new(item);
  $static(connect,items)(self, result, libui_menu_items_last(self));
  return result;
}

void self_append_separator(libui_menu *self)
{
  uiMenuAppendSeparator(WIDGET);
}

$method libui_menu_item *self_append_about_item(libui_menu *self)
{
  return append(self, uiMenuAppendAboutItem(WIDGET));
}

$method libui_menu_item *self_append_preferences_item(libui_menu *self)
{
  return append(self, uiMenuAppendPreferencesItem(WIDGET));
}

$method libui_menu_item *self_append_quit_item(libui_menu *self)
{
  return append(self, uiMenuAppendQuitItem(WIDGET));
}

$method libui_menu_item *self_append_check_item(libui_menu *self, goo_string name)
{
  return append(self, uiMenuAppendCheckItem(WIDGET, goo_string_data(name)));
}

$method libui_menu_item *self_append_item(libui_menu *self, goo_string name)
{
  return append(self, uiMenuAppendItem(WIDGET, goo_string_data(name)));
}
