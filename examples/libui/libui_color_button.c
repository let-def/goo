#include "libui_color_button.h"
#define WIDGET uiColorButton($field(self, control))

static void on_changed(uiColorButton *b, void *self)
{
  $static(event, changed)((libui_color_button*)self);
}

$method libui_color_button *self_new(void)
{
  libui_color_button *self = $alloc();
  $field(self, control) = uiControl(uiNewColorButton());
  uiColorButtonOnChanged(WIDGET, on_changed, self);
  return self;
}

$method void self_set_color(libui_color_button *self, double r, double g, double b, double a)
{
  uiColorButtonSetColor(WIDGET, r, g, b, a);
}

$method void self_get_color(libui_color_button *self, double *ret0, double *ret1, double *ret2, double *ret3)
{
  uiColorButtonColor(WIDGET, ret0, ret1, ret2, ret3);
}
