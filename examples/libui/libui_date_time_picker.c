#include "libui_date_time_picker.h"
#define WIDGET uiDateTimePicker($field(self, control))

static libui_date_time_picker *init(uiDateTimePicker *control)
{
  libui_date_time_picker *self = $alloc();
  $field(self, control) = uiControl(control);
  return self;
}

$method libui_date_time_picker *self_new_time(void)
{
  return init(uiNewTimePicker());
}

$method libui_date_time_picker *self_new_date(void)
{
  return init(uiNewDatePicker());
}

$method libui_date_time_picker *self_new(void)
{
  return init(uiNewDateTimePicker());
}
