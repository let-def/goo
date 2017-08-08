#include "libui_spinbox.h"
#define WIDGET uiSpinbox($field(self, control))

static void on_changed(uiSpinbox *e, void *self)
{
  $static(event, changed)((libui_spinbox*)self);
}

$method libui_spinbox *self_new(int min, int max)
{
  libui_spinbox *self = $alloc();
  $field(self, control) = uiControl(uiNewSpinbox(min, max));
  uiSpinboxOnChanged(WIDGET, on_changed, self);
  return self;
}

$method void self_set_value(libui_spinbox *self, int value)
{
  uiSpinboxSetValue(WIDGET, value);
}

$method int self_value(libui_spinbox *self)
{
  return uiSpinboxValue(WIDGET);
}
