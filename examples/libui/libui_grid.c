#include "libui_grid.h"
#define WIDGET uiGrid($field(self, control))

libui_grid *libui_grid_new(void)
{
  libui_grid *self = $alloc();
  $field(self, control) = uiControl(uiNewGrid());
  return self;
}

goo_bool libui_grid_is_padded(libui_grid *self)
{
  return uiGridPadded(WIDGET);
}

void libui_grid_set_padded(libui_grid *self, goo_bool padded)
{
  uiGridSetPadded(WIDGET, padded);
}

void libui_grid_insert_at(libui_grid *self, libui_control *c, libui_control *existing, libui_at at, int xspan, int yspan, goo_bool hexpand, libui_align halign, goo_bool vexpand, libui_align valign)
{
  $static(connect,children)(self, c, NULL);
  uiGridInsertAt(WIDGET, $field(c, control), $field(existing, control),
      at, xspan, yspan, hexpand, halign, vexpand, valign);
}

void libui_grid_append(libui_grid *self, libui_control *c, int left, int top, int xspan, int yspan, goo_bool hexpand, libui_align halign, goo_bool vexpand, libui_align valign)
{
  $static(connect,children)(self, c, NULL);
  uiGridAppend(WIDGET, $field(c, control), left, top,
      xspan, yspan, hexpand, halign, vexpand, valign);
}
