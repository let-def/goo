#ifndef __ML_GOO_H__
#define __ML_GOO_H__

#include "caml/mlvalues.h"
#include "goo_system.h"

typedef struct {
  void *block;
  int fill;
} goo_region_t;

goo_region_t goo_region_enter(void);
void goo_region_leave(goo_region_t region);
value *goo_region_alloc(void);

#define GOO_ENTER_REGION region_t goo_region = goo_region_enter()
#define GOO_LEAVE_REGION goo_region_leave(goo_region)

goo_object *Goo_val(value v);
#define $Goo_val(goo, typ) $cast(Goo_val(goo), typ)

value Val_goo(goo_object *goo);
#define $Val_goo(goo) Val_goo($as(goo, goo_object))

value Val_goo_option(goo_object *goo);
#define $Val_goo_option(goo) Val_goo_option($as(goo, goo_object))

value Val_goo_alloc(goo_object *goo);
value Val_goo_get(goo_object *goo);

value Val_goo_handler_helper(goo_object *goo);
#define $Val_goo_handler_helper(goo) Val_goo_handler_helper($as(goo, goo_object))

void ml_goo_set_property(goo_object *goo, unsigned int prop_id, goo_object *val);
#define $ml_goo_set_property(goo, prop, val) \
    ml_goo_set_property($as(goo, goo_object), prop, $as(val, goo_object))

goo_string Goo_string_val_(value *str);
#define Goo_string_val(v) Goo_string_val_(&(v))

value Val_goo_string(goo_string str);
value Val_goo_string_release(goo_string str);

value ml_goo_cast(value object, value witness);

void ml_goo_port_connect(goo_object *source, unsigned int collection_prop, goo_object *target, unsigned int target_prop);
#define $ml_goo_port_connect(source, collection_prop, target, target_prop) \
  ml_goo_port_connect($as(source, goo_object), collection_prop, $as(target, goo_object), target_prop)

/* Goo disconnect is a bit trickier:
 * The target object has at least one reference up until that point.
 * But after execution of disconnect, it might be orphaned and at the risk of
 * being collected.
 * However, the reference is still needed for firing the on_disconnect event.
 * ml_goo_port_disconnect will invoke the callback and ensures that the target
 * is still reachable during the execution of the callback.
 */
void ml_goo_port_disconnect(goo_object *source, unsigned int collection_prop, goo_object *target, unsigned int target_prop, void (*callback)(goo_object *source, goo_object *target));
#define $ml_goo_port_disconnect(source, collection_prop, target, target_prop, callback) \
  do {                                                                                  \
    /* Don't call anything but ensure that the call typechecks */                       \
    if (0) (callback)(source, target);                                                  \
    ml_goo_port_disconnect($as(source, goo_object), collection_prop,                    \
                           $as(target, goo_object), target_prop,                        \
                           (void*)(callback));                                          \
  } while(0)

#define GOO_STUB_PORT(target, target_field, source)            \
  value ml_##target##_##target_field##_get(value vtarget)      \
  {                                                            \
    CAMLparam1(vtarget);                                       \
    target* atarget = $Goo_val(vtarget, target);               \
    goo_object *result = $field(atarget, target_field).parent; \
    if (result) goo_assert ($cast(result, source));            \
    CAMLreturn($Val_goo_option(result));                       \
  }                                                            \
                                                               \
  value ml_##target##_##target_field##_detach(value vtarget)   \
  {                                                            \
    CAMLparam1(vtarget);                                       \
    target* atarget = $Goo_val(vtarget, target);               \
    goo_assert (atarget);                                      \
    target##_##target_field##_disconnect(atarget);             \
    CAMLreturn(Val_unit);                                      \
  }

#define GOO_STUB_SLOT(source, source_field, target, target_field) \
  value ml_##source##_##source_field##_get(value vsource)         \
  {                                                               \
    CAMLparam1(vsource);                                          \
    source *asource = $Goo_val(vsource, source);                  \
    target *result = $field(asource, source_field);               \
    CAMLreturn($Val_goo_option(result));                          \
  }

#define GOO_STUB_COLLECTION_(name, source, source_field, target, target_field)             \
  value ml_##name##prev(value vtarget);                                                    \
  value ml_##name##next(value vtarget);                                                    \
  value ml_##name##first(value vsource);                                                   \
  value ml_##name##last(value vsource);                                                    \
  value ml_##name##parent(value vtarget);                                                  \
                                                                                           \
  value ml_##name##prev(value vtarget)                                                     \
  {                                                                                        \
    CAMLparam1(vtarget);                                                                   \
    target* atarget = $Goo_val(vtarget, target);                                           \
    goo_assert (atarget);                                                                  \
    target* result = name##prev(atarget);                                                  \
    CAMLreturn($Val_goo_option(result));                                                   \
  }                                                                                        \
                                                                                           \
  value ml_##name##next(value vtarget)                                                     \
  {                                                                                        \
    CAMLparam1(vtarget);                                                                   \
    target* atarget = $Goo_val(vtarget, target);                                           \
    goo_assert (atarget);                                                                  \
    target* result = name##next(atarget);                                                  \
    CAMLreturn($Val_goo_option(result));                                                   \
  }                                                                                        \
                                                                                           \
  value ml_##name##first(value vsource)                                                    \
  {                                                                                        \
    CAMLparam1(vsource);                                                                   \
    source* asource = $Goo_val(vsource, source);                                           \
    goo_object *result = $field(asource, source_field).first;                              \
    if (result) goo_assert($cast(result, target));                                         \
    CAMLreturn($Val_goo_option(result));                                                   \
  }                                                                                        \
                                                                                           \
  value ml_##name##last(value vsource)                                                     \
  {                                                                                        \
    CAMLparam1(vsource);                                                                   \
    source* asource = $Goo_val(vsource, source);                                           \
    goo_object *result = $field(asource, source_field).last;                               \
    if (result) goo_assert($cast(result, target));                                         \
    CAMLreturn($Val_goo_option(result));                                                   \
  }                                                                                        \
                                                                                           \
  value ml_##name##parent(value vtarget)                                                   \
  {                                                                                        \
    CAMLparam1(vtarget);                                                                   \
    CAMLreturn($Val_goo_option(name##parent($Goo_val(vtarget, target))));                  \
  }

#define GOO_STUB_COLLECTION(source, source_field, target, target_field) \
  GOO_STUB_COLLECTION_(source##_##source_field##_, source, source_field, target, target_field)

#endif /* !__ML_GOO_H__ */
