#include "libui_font_button.h"
#define WIDGET uiFontButton($field(self, control))

static void on_changed(uiFontButton *b, void *self)
{
  $static(self, on_changed)((libui_font_button*)self);
}

libui_font_button *libui_font_button_new(void)
{
  libui_font_button *self = $alloc();
  $field(self, control) = uiControl(uiNewFontButton());
  uiFontButtonOnChanged(WIDGET, on_changed, self);
  return self;
}
