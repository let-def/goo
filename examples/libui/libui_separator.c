#include "libui_separator.h"
#define WIDGET uiSeparator($field(self, control))

static libui_separator *init(uiSeparator *sep)
{
  libui_separator *self = $alloc();
  $field(self, control) = uiControl(sep);
  return self;
}

libui_separator *libui_separator_new_vertical(void)
{
  return init(uiNewVerticalSeparator());
}

libui_separator *libui_separator_new_horizontal(void)
{
  return init(uiNewHorizontalSeparator());
}
