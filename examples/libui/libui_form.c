#include "libui_form.h"
#define WIDGET uiForm($field(self, control))

$method libui_form *self_new(void)
{
 libui_form *self = $alloc();
 $field(self, control) = uiControl(uiNewForm());
 return self;
}

$method goo_bool self_is_padded(libui_form *self)
{
  return uiFormPadded(WIDGET);
}

$method void self_set_padded(libui_form *self, goo_bool padded)
{
  return uiFormSetPadded(WIDGET, padded);
}

$method void self_delete(libui_form *self, libui_control *child)
{
  int position = -1;
  for (libui_control *iter = child; iter; iter = libui_form_children_prev(iter))
    position++;
  uiFormDelete(WIDGET, position);
  libui_control_parent_disconnect(child);
}

$method void self_append(libui_form *self, goo_string name, libui_control *c, goo_bool stretchy)
{
  $static(connect,children)(self, c, libui_form_children_last(self));
  uiFormAppend(WIDGET, goo_string_data(name), $field(c, control), stretchy);
}
