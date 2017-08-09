#include "libui_separator.h"
#define WIDGET uiSeparator($field(self, control))

static libui_separator *init(uiSeparator *sep)
{
  libui_separator *self = $alloc();
  $field(self, control) = uiControl(sep);
  return self;
}

$method libui_separator *self_new_vertical(void)
{
  return init(uiNewVerticalSeparator());
}

$method libui_separator *self_new_horizontal(void)
{
  return init(uiNewHorizontalSeparator());
}
