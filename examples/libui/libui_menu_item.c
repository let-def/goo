#include "libui_menu_item.h"
#define WIDGET uiMenuItem($field(self, control))

static void on_clicked(uiMenuItem *sender, uiWindow *window, void *self)
{
  $static(event, clicked)((libui_menu_item*)self);
}

$method libui_menu_item *self_new(uiMenuItem * item)
{
  libui_menu_item *self = $alloc();
  $field(self, control) = item;
  uiMenuItemOnClicked(WIDGET, on_clicked, self);
  return self;
}

$method goo_bool self_is_checked(libui_menu_item *self)
{
  return uiMenuItemChecked(WIDGET);
}

$method void self_set_checked(libui_menu_item *self, goo_bool checked)
{
  uiMenuItemSetChecked(WIDGET, checked);
}

$method void self_disable(libui_menu_item *self)
{
  uiMenuItemDisable(WIDGET);
}

$method void self_enable(libui_menu_item *self)
{
  uiMenuItemEnable(WIDGET);
}
