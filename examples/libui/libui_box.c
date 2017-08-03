#include "libui_box.h"
#define WIDGET uiBox($field(self, control))

static libui_box *init(uiBox *widget)
{
  libui_box *self = $alloc();
  $field(self, control) = uiControl(widget);
  return self;
}

libui_box *libui_box_new_vertical(void)
{
  return init(uiNewVerticalBox());
}

libui_box *libui_box_new_horizontal(void)
{
  return init(uiNewHorizontalBox());
}

goo_bool libui_box_is_padded(libui_box *self)
{
  return uiBoxPadded(WIDGET);
}

void libui_box_set_padded(libui_box *self, goo_bool padded)
{
  uiBoxSetPadded(WIDGET, padded);
}

void libui_box_append(libui_box *self, libui_control *child, goo_bool stretchy)
{
  $static(connect,children)(self, child, libui_box_children_last(self));
  uiBoxAppend(WIDGET, $field(child, control), stretchy);
}
