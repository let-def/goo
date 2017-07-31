#include "libui_form.h"
#define WIDGET uiForm($field(self, control))

libui_form *libui_form_new(void)
{
 libui_form *self = $alloc();
 $field(self, control) = uiControl(uiNewForm());
 return self;
}

goo_bool libui_form_is_padded(libui_form *self)
{
  return uiFormPadded(WIDGET);
}

void libui_form_set_padded(libui_form *self, goo_bool padded)
{
  return uiFormSetPadded(WIDGET, padded);
}

void libui_form_delete(libui_form *self, libui_control *child)
{
  int position = -1;
  for (libui_control *iter = child; iter; iter = libui_form_children_prev(iter))
    position++;
  uiFormDelete(WIDGET, position);
  libui_control_parent_disconnect(child);
}

void libui_form_append(libui_form *self, goo_string name, libui_control *c, goo_bool stretchy)
{
  _connect_children(self, c, libui_form_children_last(self));
  uiFormAppend(WIDGET, goo_string_data(name), $field(c, control), stretchy);
}
