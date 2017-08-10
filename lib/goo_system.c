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

void goo_object_destroy(goo_object *self)
{
  $send(self, destroy)(self);
}

void static_goo_object_destroy(goo_object *self)
{
  free(self);
}
