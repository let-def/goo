#include "libui_control.h"
#define WIDGET $field(self, control)

void libui_control_disable(libui_control *self)
{
  uiControlDisable(WIDGET);
}

void libui_control_enable(libui_control *self)
{
  uiControlEnable(WIDGET);
}

goo_bool libui_control_is_enabled(libui_control *self)
{
  return uiControlEnabled(WIDGET);
}

void libui_control_hide(libui_control *self)
{
  uiControlHide(WIDGET);
}

void libui_control_show(libui_control *self)
{
  uiControlShow(WIDGET);
}

goo_bool libui_control_is_visible(libui_control *self)
{
  return uiControlVisible(WIDGET);
}

goo_bool libui_control_is_toplevel(libui_control *self)
{
  return uiControlToplevel(WIDGET);
}

void libui_control_destroy(libui_control *self)
{
  uiControlDestroy(WIDGET);
  $static(super, destroy)(self);
}
