#include "libui_menu_item.h"
#define WIDGET uiMenuItem($field(self, control))

static void on_clicked(uiMenuItem *sender, uiWindow *window, void *self)
{
  $static(self, on_clicked)((libui_menu_item*)self);
}

libui_menu_item *libui_menu_item_new(uiMenuItem * item)
{
  libui_menu_item *self = $alloc();
  $field(self, control) = item;
  uiMenuItemOnClicked(WIDGET, on_clicked, self);
  return self;
}

goo_bool libui_menu_item_is_checked(libui_menu_item *self)
{
  return uiMenuItemChecked(WIDGET);
}

void libui_menu_item_set_checked(libui_menu_item *self, goo_bool checked)
{
  uiMenuItemSetChecked(WIDGET, checked);
}

void libui_menu_item_disable(libui_menu_item *self)
{
  uiMenuItemDisable(WIDGET);
}

void libui_menu_item_enable(libui_menu_item *self)
{
  uiMenuItemEnable(WIDGET);
}
