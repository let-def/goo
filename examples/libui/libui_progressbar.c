#include "libui_progressbar.h"
#define WIDGET uiProgressBar($field(self, control))

libui_progressbar *libui_progressbar_new(void)
{
  libui_progressbar *self = $alloc();
  $field(self, control) = uiControl(uiNewProgressBar());
  return self;
}

void libui_progressbar_set_value(libui_progressbar *self, int value)
{
  uiProgressBarSetValue(WIDGET, value);
}

int libui_progressbar_value(libui_progressbar *self)
{
  return uiProgressBarValue(WIDGET);
}
