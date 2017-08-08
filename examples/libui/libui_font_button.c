#include "libui_font_button.h"
#define WIDGET uiFontButton($field(self, control))

static void on_changed(uiFontButton *b, void *self)
{
  $static(event, changed)((libui_font_button*)self);
}

$method libui_font_button *self_new(void)
{
  libui_font_button *self = $alloc();
  $field(self, control) = uiControl(uiNewFontButton());
  uiFontButtonOnChanged(WIDGET, on_changed, self);
  return self;
}
