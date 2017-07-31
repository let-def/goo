#include "libui_slider.h"
#define WIDGET uiSlider($field(self, control))

static void on_changed(uiSlider *e, void *self)
{
  $static(self, on_changed)((libui_slider*)self);
}

libui_slider *libui_slider_new(int min, int max)
{
  libui_slider *self = $alloc();
  $field(self, control) = uiControl(uiNewSlider(min, max));
  uiSliderOnChanged(WIDGET, on_changed, self);
  return self;
}

void libui_slider_set_value(libui_slider *self, int value)
{
  uiSliderSetValue(WIDGET, value);
}

int libui_slider_value(libui_slider *self)
{
  return uiSliderValue(WIDGET);
}
