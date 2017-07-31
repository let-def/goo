#include "libui_group.h"
#define WIDGET uiGroup($field(self, control))

libui_group *libui_group_new(goo_string title)
{
  libui_group *self = $alloc();
  $field(self, control) = uiControl(uiNewGroup(goo_string_data(title)));
  return self;
}

goo_bool libui_group_is_margined(libui_group *self)
{
  return uiGroupMargined(WIDGET);
}

void libui_group_set_margined(libui_group *self, goo_bool margined)
{
  uiGroupSetMargined(WIDGET, margined);
}

void libui_group_set_title(libui_group *self, goo_string title)
{
  uiGroupSetTitle(WIDGET, goo_string_data(title));
}

goo_string libui_group_title(libui_group *self)
{
  return goo_string_from_c(uiGroupTitle(WIDGET), 0);
}

void libui_group_child_connect(libui_group *self, libui_control *child)
{
  $static(connect, child)(self, child);
  uiGroupSetChild(WIDGET, $field($field(self, child), control));
}

void libui_group_on_child_disconnect(libui_group *self, libui_control *object)
{
  uiGroupSetChild(WIDGET, NULL);
}

