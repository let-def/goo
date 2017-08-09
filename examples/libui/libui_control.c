#include "libui_control.h"
#define WIDGET $field(self, control)

$method void self_disable(libui_control *self)
{
  uiControlDisable(WIDGET);
}

$method void self_enable(libui_control *self)
{
  uiControlEnable(WIDGET);
}

$method goo_bool self_is_enabled(libui_control *self)
{
  return uiControlEnabled(WIDGET);
}

$method void self_hide(libui_control *self)
{
  uiControlHide(WIDGET);
}

$method void self_show(libui_control *self)
{
  uiControlShow(WIDGET);
}

$method goo_bool self_is_visible(libui_control *self)
{
  return uiControlVisible(WIDGET);
}

$method goo_bool self_is_toplevel(libui_control *self)
{
  return uiControlToplevel(WIDGET);
}

//$method void self_destroy(libui_control *self)
//{
//  uiControlDestroy(WIDGET);
//  $static(super, destroy)(self);
//}
