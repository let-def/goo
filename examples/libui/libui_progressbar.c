#include "libui_progressbar.h"
#define WIDGET uiProgressBar($field(self, control))

$method libui_progressbar *self_new(void)
{
  libui_progressbar *self = $alloc();
  $field(self, control) = uiControl(uiNewProgressBar());
  return self;
}

$method void self_set_value(libui_progressbar *self, int value)
{
  uiProgressBarSetValue(WIDGET, value);
}

$method int self_value(libui_progressbar *self)
{
  return uiProgressBarValue(WIDGET);
}
