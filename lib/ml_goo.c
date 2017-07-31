#include "ml_goo.h"
#include <stdio.h>

#include "caml/alloc.h"
#include "caml/memory.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "goo_system.h"

static value Val_some(value v)
{
  CAMLparam1(v);
  CAMLlocal1(ret);
  ret = caml_alloc_small(1, 0);
  Field(ret, 0) = v;
  CAMLreturn(ret);
}

goo_object *Goo_val(value handle)
{
  return *(goo_object**)(Data_custom_val(Field(handle,0)));
}

value ml_goo_set_handle(value handle, value index)
{
  goo_object *obj = Goo_val(handle);
  goo_object_set_handle(obj, (void*)Long_val(index));
  return Val_unit;
}

value ml_goo_get_handle(value handle)
{
  goo_object *obj = Goo_val(handle);
  return Val_long(goo_object_get_handle(obj));
}

static void goo_finalize(value handle)
{
  goo_object *obj = *(goo_object **)(Data_custom_val(handle));
  const goo_class_display *display = $send(obj, display_);
  printf("collecting %s\n", display->witnesses[display->depth-1]->name);
  $goo_object_destroy(obj);
}

static int goo_compare(value v1, value v2)
{
  goo_object *o1 = *(goo_object **)(Data_custom_val(v1));
  goo_object *o2 = *(goo_object **)(Data_custom_val(v2));

  if (o1 < o2) return -1;
  if (o1 > o2) return 1;
  return 0;
}

value ml_goo_compare(value v1, value v2)
{
  return Val_long(goo_compare(Field(v1, 0), Field(v2, 0)));
}

static intnat goo_hash(value v)
{
  goo_object *obj = *(goo_object **)(Data_custom_val(v));
  return (intnat)obj;
}

value ml_goo_hash(value v1)
{
  return Val_long(goo_hash(Field(v1, 0)));
}

static struct custom_operations goo_custom_ops = {
    identifier: "goo object",
    finalize:    goo_finalize,
    compare:     goo_compare,
    hash:        goo_hash,
    serialize:   custom_serialize_default,
    deserialize: custom_deserialize_default
};

value Val_goo_alloc(goo_object *goo)
{
  CAMLparam0();
  CAMLlocal2(custom, block);
  int i, count;

  if ($field(goo, handle_) != NULL) abort();

  custom = caml_alloc_custom(&goo_custom_ops, sizeof(goo_object *), 0, 1);
  *(goo_object**)(Data_custom_val(custom)) = goo;

  count = 2 + $number_of_properties(goo);
  block = caml_alloc_tuple(count);
  Field(block, 0) = custom;

  for (i = 1; i < count; ++i)
    Field(block, i) = Val_unit;

  static value * alloc_id = NULL;
  if (alloc_id == NULL) {
    alloc_id = caml_named_value("ml_goo_alloc");
  }

  value result = caml_callback(*alloc_id, block);
  if (Is_exception_result(result)) abort();

  printf("allocated id %ld\n", Long_val(result));
  goo_object_set_handle(goo, (void*)result);

  CAMLreturn(block);
}

value Val_goo_get(goo_object *goo)
{
  static value *deref = NULL;
  if (deref == NULL) {
    deref = caml_named_value("ml_goo_deref");
  }

  if ($field(goo, handle_) == NULL) abort();

  printf("accessing id %ld\n", Long_val((value)goo_object_get_handle(goo)));
  value result = caml_callback(*deref, (value)goo_object_get_handle(goo));
  if (Is_exception_result(result)) abort();

  return result;
}

value Val_goo(goo_object *goo)
{
  if ($field(goo, handle_) == NULL)
    return Val_goo_alloc(goo);
  else
    return Val_goo_get(goo);
}

value Val_goo_option(goo_object *goo)
{
  if (goo == NULL)
    return Val_unit;
  else
    return Val_some(Val_goo(goo));
}

value Val_goo_handler_helper(goo_object *goo)
{
  if ($field(goo, handle_) == NULL)
    return Val_unit;
  value inst = Val_goo_get(goo);
  if (Field(inst, 1) == Val_unit)
    return Val_unit;
  return inst;
}

void ml_goo_set_property(goo_object *goo, unsigned int prop_id, goo_object *val)
{
  CAMLparam0();
  CAMLlocal2(vgoo, vval);

  vgoo = Val_goo_get(goo);
  vval = (val == NULL) ? Val_unit : Val_goo(val);

  if (prop_id + 2 >= Wosize_val(vgoo)) abort();

  Store_field(vgoo, prop_id + 2, vval);

  CAMLreturn0;
}

static const char *ml_string_data(void *v)
{
  value *ml = v;
  return String_val(*ml);
}

static size_t ml_string_length(void *v)
{
  value *ml = v;
  return caml_string_length(*ml);
}
static goo_string_class ml_string = {
  .data = ml_string_data,
  .length = ml_string_length,
  .release = NULL,
};

goo_string Goo_string_val_(value *str)
{
  goo_string result;
  result.value = str;
  result.table = &ml_string;
  return result;
}

static value Val_goo_string_gen(goo_string str, goo_bool release)
{
  CAMLparam0();
  CAMLlocal1(v);
  size_t len;

  if (str.table == &ml_string)
    return *(value*)str.value;

  len = goo_string_length(str);
  v = caml_alloc_string(len);
  memcpy(String_val(v), goo_string_data(str), len);

  if (release) goo_string_release(str);

  CAMLreturn(v);
}

value Val_goo_string(goo_string str)
{
  return Val_goo_string_gen(str, 0);
}

value Val_goo_string_release(goo_string str)
{
  return Val_goo_string_gen(str, 1);
}

value ml_goo_cast(value vobject, value vwitness)
{
  goo_object *o = Goo_val(vobject);
  goo_class_witness *w = (void*)((intnat)(vwitness) & (~1));
  return Val_goo_option(goo_dyncast_(o, w));
}

void ml_goo_port_connect(goo_object *source, unsigned int table_prop, goo_object *target, unsigned int port_prop)
{
  CAMLparam0();
  CAMLlocal3(vsource, vtarget, vnext);
  vsource = Val_goo(source);
  vtarget = Val_goo(target);
  vnext = Field(vsource, table_prop + 2);

  goo_assert(Field(vtarget, port_prop + 2) == Val_unit);
  goo_assert(Field(vtarget, port_prop + 3) == Val_unit);

  Store_field(vsource, table_prop + 2, vtarget);
  Store_field(vtarget, port_prop + 2, vsource);
  Store_field(vtarget, port_prop + 3, vnext);

  if (vnext != Val_unit)
    Store_field(vnext, port_prop +2, vtarget);

  CAMLreturn0;
}

void ml_goo_port_disconnect(goo_object *source, unsigned int table_prop, goo_object *target, unsigned int port_prop, void (*callback)(goo_object *source, goo_object *target))
{
  CAMLparam0();
  CAMLlocal4(vsource, vtarget, vprev, vnext);
  vsource = Val_goo(source);
  vtarget = Val_goo(target);
  vprev = Field(vtarget,port_prop+2);
  vnext = Field(vtarget,port_prop+3);

  if (Field(vsource, table_prop+2) == vtarget)
  { // First in source, update to next
    goo_assert(vprev == vsource);
    Store_field(vsource, table_prop+2, vnext);
    if (vnext != Val_unit)
      Store_field(vnext, port_prop+2, vsource);
  }
  else
  {
    goo_assert(vprev != Val_unit);
    Store_field(vprev, port_prop+3, vnext);
    if (vnext != Val_unit)
      Store_field(vnext, port_prop+2, vprev);
  }

  Store_field(vtarget, port_prop+2, Val_unit);
  Store_field(vtarget, port_prop+3, Val_unit);

  if (callback) callback(source, target);
  CAMLreturn0;
}
