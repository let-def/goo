#include <stdlib.h>
#include "goo_types.h"

const char *goo_string_data(goo_string str)
{
  if (str.table && str.table->data)
    return str.table->data(str.value);
  else
    return "";
}

int goo_string_length(goo_string str)
{
  if (str.table && str.table->length)
    return str.table->length(str.value);
  else
    return 0;
}

void goo_string_release(goo_string str)
{
  if (str.table && str.table->release)
    str.table->release(str.value);
}

static const char *c_string_data(void *value)
{
  return value;
}

static size_t c_string_length(void *value)
{
  return strlen(value);
}

static goo_string_class c_string_freeable = {
  .data = c_string_data,
  .length = c_string_length,
  .release = free,
};

static goo_string_class c_string_permanent = {
  .data = c_string_data,
  .length = c_string_length,
  .release = NULL,
};

goo_string null_string = {
  .value = NULL,
  .table = NULL,
};

goo_string goo_string_from_c(const char *string, goo_bool free)
{
  goo_string result;
  result.value = (void*)string;
  result.table = free ? &c_string_freeable : &c_string_permanent;
  return result;
}
