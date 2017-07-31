#include "libui_date_time_picker.h"
#define WIDGET uiDateTimePicker($field(self, control))

static libui_date_time_picker *init(uiDateTimePicker *control)
{
  libui_date_time_picker *self = $alloc();
  $field(self, control) = uiControl(control);
  return self;
}

libui_date_time_picker *libui_date_time_picker_new_time(void)
{
  return init(uiNewTimePicker());
}

libui_date_time_picker *libui_date_time_picker_new_date(void)
{
  return init(uiNewDatePicker());
}

libui_date_time_picker *libui_date_time_picker_new(void)
{
  return init(uiNewDateTimePicker());
}
