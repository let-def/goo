#include "libui_group.h"
#define WIDGET uiGroup($field(self, control))

$method libui_group *self_new(goo_string title)
{
  libui_group *self = $alloc();
  $field(self, control) = uiControl(uiNewGroup(goo_string_data(title)));
  return self;
}

$method goo_bool self_is_margined(libui_group *self)
{
  return uiGroupMargined(WIDGET);
}

$method void self_set_margined(libui_group *self, goo_bool margined)
{
  uiGroupSetMargined(WIDGET, margined);
}

$method void self_set_title(libui_group *self, goo_string title)
{
  uiGroupSetTitle(WIDGET, goo_string_data(title));
}

$method goo_string self_title(libui_group *self)
{
  return goo_string_from_c(uiGroupTitle(WIDGET));
}

$method void self_child_connect(libui_group *self, libui_control *child)
{
  $static(connect, child)(self, child);
  uiGroupSetChild(WIDGET, $field($field(self, child), control));
}

$method void self_on_child_disconnect(libui_group *self, libui_control *object)
{
  uiGroupSetChild(WIDGET, NULL);
}

