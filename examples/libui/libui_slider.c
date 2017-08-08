#include "libui_slider.h"
#define WIDGET uiSlider($field(self, control))

static void on_changed(uiSlider *e, void *self)
{
  $static(event, changed)((libui_slider*)self);
}

$method libui_slider *self_new(int min, int max)
{
  libui_slider *self = $alloc();
  $field(self, control) = uiControl(uiNewSlider(min, max));
  uiSliderOnChanged(WIDGET, on_changed, self);
  return self;
}

$method void self_set_value(libui_slider *self, int value)
{
  uiSliderSetValue(WIDGET, value);
}

$method int self_value(libui_slider *self)
{
  return uiSliderValue(WIDGET);
}
