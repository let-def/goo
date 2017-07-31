#include "libui_color_button.h"
#define WIDGET uiColorButton($field(self, control))

static void on_changed(uiColorButton *b, void *self)
{
  $static(self, on_changed)((libui_color_button*)self);
}

libui_color_button *libui_color_button_new(void)
{
  libui_color_button *self = $alloc();
  $field(self, control) = uiControl(uiNewColorButton());
  uiColorButtonOnChanged(WIDGET, on_changed, self);
  return self;
}

void libui_color_button_set_color(libui_color_button *self, double r, double g, double b, double a)
{
  uiColorButtonSetColor(WIDGET, r, g, b, a);
}
