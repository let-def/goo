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

goo_object *Goo_val_option(value handle)
{
  if (handle == Val_unit)
    return NULL;
  else
    return Goo_val(Field(handle,0));
}

value ml_goo_set_handle(value handle, value index)
{
  goo_assert ((index & 1) != 0);
  goo_object *obj = Goo_val(handle);
  $field(obj, handle_) = (void*)index;
  return Val_unit;
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

static value Val_goo_alloc(goo_object *goo)
{
  value block, *root = goo_region_alloc();

  *root = caml_alloc_custom(&goo_custom_ops, sizeof(goo_object *), 0, 1);
  *(goo_object**)(Data_custom_val(*root)) = goo;

  block = caml_alloc_tuple(2 + $number_of_properties(goo));
  Field(block, 0) = *root;
  *root = block;

  static value * alloc_id = NULL;
  if (alloc_id == NULL)
    alloc_id = caml_named_value("ml_goo_alloc");

  value result = caml_callback_exn(*alloc_id, block);
  if (Is_exception_result(result)) abort();

  printf("allocated id %ld\n", Long_val(result));
  goo_assert((result & 1) != 0);
  $field(goo, handle_) = (void*)result;

  return block;
}

value Val_goo(goo_object *goo)
{
  if ($field(goo, handle_) == NULL)
    return Val_goo_alloc(goo);

  static value *deref = NULL;
  if (deref == NULL)
    deref = caml_named_value("ml_goo_deref");

  printf("accessing id %ld\n", Long_val((value)$field(goo, handle_)));
  value result = caml_callback_exn(*deref, (value)$field(goo, handle_));
  if (Is_exception_result(result)) abort();

  return result;
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
  value inst = Val_goo(goo);
  if (Field(inst, 1) == Val_unit)
    return Val_unit;
  return inst;
}

void ml_goo_set_property(goo_object *goo, unsigned int prop_id, goo_object *val)
{
  CAMLparam0();
  CAMLlocal2(vgoo, vval);

  vgoo = Val_goo(goo);
  vval = (val == NULL) ? Val_unit : Val_goo(val);

  if (prop_id + 2 >= Wosize_val(vgoo)) abort();

  Store_field(vgoo, prop_id + 2, vval);

  CAMLreturn0;
}

const char *goo_string_data(goo_string str)
{
  return String_val(*(value*)str.data);
}

int goo_string_length(goo_string str)
{
  return caml_string_length(*(value*)str.data);
}

goo_string Goo_string_val(value str)
{
  value *v = goo_region_alloc();
  *v = str;
  return (goo_string){ .data = v };
}

value Val_goo_string(goo_string str)
{
  if (str.data)
    return *(value*)str.data;

  static value *string_null = NULL;
  if (string_null == NULL)
    string_null = caml_named_value("ml_goo_string");

  return *string_null;
}

goo_string null_string;

goo_string goo_string_from_c(const char *string)
{
  return Goo_string_val(caml_copy_string(string ? string : ""));
}

goo_string goo_string_from_mem(const char *string, size_t len)
{
  value v = caml_alloc_string(len);
  memcpy(String_val(v), string, len);
  return Goo_string_val(v);
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

typedef struct {
  struct caml__roots_block desc;
  value values[1024];
} region_t;

static struct caml__roots_block root_sentinel = { .nitems = 0, .ntables = 0, .next = NULL };

region_t *region_root, *spare_region;

static region_t *region_alloc()
{
  region_t *result;
  if (spare_region)
  {
    result = spare_region;
    spare_region = NULL;
  }
  else
    result = malloc(sizeof(region_t) + sizeof(value) * 1024);

  goo_assert (result);
  result->desc.next = NULL;
  result->desc.nitems = 0;
  result->desc.ntables = 1;
  result->desc.tables[0] = result->values;

  return result;
}

static void region_release(region_t *region)
{
  if (spare_region)
    free(region);
  else
    spare_region = region;
}

goo_region_t goo_region_enter(void)
{
  if (region_root == NULL)
  {
    region_root = region_alloc();
    region_root->desc.next = caml_local_roots;

    root_sentinel.next = &region_root->desc;
    caml_local_roots = &root_sentinel;
    return (goo_region_t){ .block = region_root, .fill = -1 };
  }
  else
    return (goo_region_t){ .block = region_root, .fill = region_root->desc.nitems };
}

void goo_region_leave(goo_region_t region)
{
  goo_assert (region_root);
  while (region.block != root_sentinel.next)
  {
    region_t *current = (region_t*)root_sentinel.next;
    root_sentinel.next = current->desc.next;
    region_release(current);
  }
  if (region.fill == -1)
  {
    goo_assert (caml_local_roots == &root_sentinel);
    caml_local_roots = region_root->desc.next;
    region_release(region_root);
    region_root = NULL;
  }
  else
    root_sentinel.next->nitems = region.fill;
}

value *goo_region_alloc(void)
{
  goo_assert (region_root);

  if (root_sentinel.next->nitems < 1024)
  {
    int n = root_sentinel.next->nitems;
    root_sentinel.next->nitems += 1;
    return &((region_t*)root_sentinel.next)->values[n];
  }

  region_t *region = region_alloc();
  region->desc.next = root_sentinel.next;
  root_sentinel.next = &region->desc;
  region->desc.nitems = 1;
  region->values[0] = Val_unit;
  return &region->values[0];
}
