#include "libui_spinbox.h"
#define WIDGET uiSpinbox($field(self, control))

static void on_changed(uiSpinbox *e, void *self)
{
  $static(self, on_changed)((libui_spinbox*)self);
}

libui_spinbox *libui_spinbox_new(int min, int max)
{
  libui_spinbox *self = $alloc();
  $field(self, control) = uiControl(uiNewSpinbox(min, max));
  uiSpinboxOnChanged(WIDGET, on_changed, self);
  return self;
}

void libui_spinbox_set_value(libui_spinbox *self, int value)
{
  uiSpinboxSetValue(WIDGET, value);
}

int libui_spinbox_value(libui_spinbox *self)
{
  return uiSpinboxValue(WIDGET);
}
