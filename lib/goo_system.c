#include "goo_system.h"
#include "ml_goo.h"

GOO_INTERNAL_WITNESS(goo_object, 1);

void *goo_dyncast_(goo_object *object, const goo_class_witness *witness)
{
  if (object == NULL)
    return NULL;
  const goo_class_display *display = $send(object, display_);
  if (display->depth >= witness->depth &&
      display->witnesses[witness->depth] == witness)
    return object;
  else
    return NULL;
}

void *goo_object_get_handle(goo_object *self)
{
  return $field(self, handle_);
}

void goo_object_set_handle(goo_object *self, void *handle)
{
  $field(self, handle_) = handle;
}

void goo_object_destroy(goo_object *self)
{
  free(self);
}
