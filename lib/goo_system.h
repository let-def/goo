#ifndef __GOO_SYSTEM_H__
#define __GOO_SYSTEM_H__

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Primitive types */

typedef int goo_bool;

typedef struct {
  void *data;
} goo_string;

const char *goo_string_data(goo_string str);
int goo_string_length(goo_string str);

goo_string null_string;

goo_string goo_string_from_c(const char *string);
goo_string goo_string_from_mem(const char *string, size_t len);

/* Basic definitions of object system.
 *
 * A witness is a runtime identifier of a given class.
 * A display is an array of witnesses, one for each class of a hierarchy.
 *
 * Together, they allow to test (in O(1)) that an object is an instance of a
 * class.
 *
 * Assuming `t` is an instance of class `c`,
 * then `t.display_.witnesses[c_witness.depth] == c_witness`.
 */
typedef struct { const int depth; const char *name; } goo_class_witness;
typedef struct { const int depth; const int properties; const goo_class_witness * const (witnesses []); } goo_class_display;

/* Class interface definition
 * ==========================
 *
 * A class definition is split in many parts.
 */

#define GOO_CLASS_DECLARE(name)                         \
  typedef struct goo_##name##_class goo_##name##_class; \
  typedef struct goo_##name##_inst goo_##name##_inst;   \
  typedef union goo_##name name;                        \
  extern const goo_class_witness goo_##name##_witness;  \
  static inline name *                                  \
  goo_##name##_inst_(goo_##name##_inst *inst)           \
  { return (name *)inst; }

/* Class methods
 * -------------
 *
 * After declaring class inheritance hierarchy, one should declare the method
 * table of the class.
 *
 * The first item should be `GOO_CLASS_METHODS_INIT(my_class);`. Then all
 * methods from inherited classes and at last, all the new methods.
 *
 * GOO_CLASS_METHODS(b)
 * {
 *   GOO_CLASS_METHODS_INIT(b);
 *   // methods of object
 *   // methods of a
 *   int (*method_of_b)(b *self, int arg1);
 * }
 *
 */

#define GOO_CLASS_METHODS(name) struct goo_##name##_class

#define GOO_CLASS_METHODS_INIT(name) const goo_class_display * const display_

/* Class hierarchy
 * ---------------
 *
 * Assuming:
 * - class object
 * - class a extends object
 * - class b extends object
 *
 * Encoding for class b will look like:
 *
 * GOO_CLASS_HIERARCHY(b)
 * {
 *   GOO_CLASS_HIERARCHY_INIT(b);
 *   GOO_CLASS_INHERIT(a);
 *   GOO_CLASS_INHERIT(object);
 * }
 *
 * In practice, this boilerplate encodes the subtyping relation in C language:
 * - `GOO_CLASS_HIERARCHY_INIT(b)`
 *   means that `(new b)` and is an instance of `b` and `self`
 * - `GOO_INHERIT(a)` means that `(new b)` is an instance of `a`
 */

#define GOO_CLASS_HIERARCHY(name) union goo_##name

#define GOO_CLASS_HIERARCHY_INIT(name) goo_##name##_inst name, self

#define GOO_CLASS_INHERIT(name) goo_##name##_inst name

/* Class fields
 * ------------
 *
 * The last part is the list of runtime variables stored in an instance of the
 * class.
 * The first item should be `GOO_CLASS_FIELDS_INIT(my_class);`. Then all
 * fields from inherited classes and at last, all the new fields.
 *
 * GOO_CLASS_FIELDS(b)
 * {
 *   GOO_CLASS_FIELDS_INIT(b);
 *   // fields from object
 *   // fields from a
 *   int field_of_b;
 * }
 */

#define GOO_CLASS_FIELDS(name) struct goo_##name##_inst

#define GOO_CLASS_FIELDS_INIT(name) \
  const goo_##name##_class * const class_; \
  void * handle_


/* Class implementation
 * ====================
 *
 * Class implementation defines the C values that will be used as witness,
 * display and method table of the instances.
 *
 * It also defines the function for allocating a new instance, with
 * uninitialized fields.
 *
 */

/* Instance witness
 * ----------------
 *
 * This value defines the runtime "identity" of the class in an inheritance
 * hierarchy.
 *
 * Depth indicates how many ancestors the class has.
 * Name is just a string of the class name, accessible at runtime.
 *
 * GOO_INTERNAL_WITNESS(object, 0);
 * GOO_INTERNAL_WITNESS(a, 1);
 * GOO_INTERNAL_WITNESS(b, 2);
 */

#define GOO_INTERNAL_WITNESS(h_name, level)          \
  const goo_class_witness goo_##h_name##_witness = { \
    .depth = level,                                  \
    .name = #h_name,                                 \
  }

/* Instance diplay
 * ---------------
 *
 * This value defines the runtime "identity" of instances of this exact class
 * (subclasses have a different display).
 *
 * It contains the witnesse of all ancestors up to this class.
 * Subclasses extend the display with new witnesses.
 *
 * It must start with `GOO_INTERNAL_DISPLAY_INIT(my_object, depth)` followed
 * by a list of all witnesses.
 *
 * GOO_INTERNAL_DISPLAY(object, 0)
 * {
 *   GOO_INTERNAL_DISPLAY_INIT(object, 0)
 *   {&goo_object_witness}
 * }
 *
 * GOO_INTERNAL_DISPLAY(b, 2)
 * {
 *   GOO_INTERNAL_DISPLAY_INIT(b, 2)
 *   {&goo_object_witness, &goo_a_witness, &goo_b_witness}
 * };
 *
 */

#define GOO_INTERNAL_DISPLAY(name, level) \
  GOO_INTERNAL_WITNESS(name, level);      \
  static const goo_class_display goo_##name##_display =

#define GOO_INTERNAL_DISPLAY_INIT(name, level, props) .depth = (level+1), .properties = (props), .witnesses =

/* Instance table
 * ---------------
 *
 * This value defines the method table of instances of this exact class
 * (subclasses have a different table).
 *
 * It must starts with `GOO_INTERNAL_TABLE_INIT(my_class),` followed by
 * an occurrence of `GOO_INTERNAL_TABLE_METHOD(meth),` for each method.
 *
 * GOO_INTERNAL_TABLE(b)
 * {
 *   GOO_INTERNAL_TABLE_INIT(b),
 *   GOO_INTERNAL_TABLE_METHOD(...for each method of object...),
 *   GOO_INTERNAL_TABLE_METHOD(...for each method of a...),
 *   GOO_INTERNAL_TABLE_METHOD(method_of_b),
 * }
 */

#define GOO_INTERNAL_TABLE(name)                        \
  static const goo_##name##_class goo_##name##_class_;  \
  GOO_INTERNAL_ALLOC(name);                             \
  static const goo_##name##_class goo_##name##_class_ =

#define GOO_INTERNAL_TABLE_INIT(name) .display_ = &goo_##name##_display
#define GOO_INTERNAL_TABLE_METHOD(name) .name = (void*)self_##name

#define GOO_INTERNAL_ALLOC(name)                                              \
  static name *goo_self_alloc(void)                                           \
  {                                                                           \
    name *self = malloc(sizeof(name));                                        \
    memset(self, 0x00, sizeof(name));                                         \
    *(const goo_##name##_class**)(&self->self.class_) = &goo_##name##_class_; \
    self->self.handle_ = NULL;                                                \
    /*goo_self_init(self);*/                                                  \
    return self;                                                              \
  }

/* Definition of `goo_object`
 * ==========================
 *
 * `goo_object` is the root of class hierarchy. It is a valid object without
 * any method or field.
 */

GOO_CLASS_DECLARE(goo_object);

GOO_CLASS_METHODS(goo_object)
{
  GOO_CLASS_METHODS_INIT(goo_object);
};

GOO_CLASS_FIELDS(goo_object)
{
  GOO_CLASS_FIELDS_INIT(goo_object);
};

GOO_CLASS_HIERARCHY(goo_object)
{
  GOO_CLASS_HIERARCHY_INIT(goo_object);
};

/* Object-oriented operations
 * ====================
 *
 * $send(instance,name) does a dynamic dispatch of method `name` on `instance`.
 *
 * For instance:
 *   int x = $send(instance_of_b, method_of_b)(instance_of_b, 42);
 *
 * $field(instance,name) resolves to the field `name` of an instance.
 *
 * For instance:
 *   int x = $field(instance_of_b, field_of_b);
 *   $field(instance_of_b, field_of_b) = 42;
 *
 * $static(class, name) does a static dispatch of method `name` to a known
 * class (either self or ancestor).
 *
 * For instance:
 *   // call exactly the definition of method_of_b from b, not any override.
 *   $static(b, method_of_b)(instance_of_subclass_of_b, 42);
 *
 * $alloc()
 *
 * Allocates a new instance of the class being defined.
 * All fields are uninitialized.
 *
 * This function is visible only in the scope of class implementation, so one
 * should define and export a constructor function making use of this.
 *
 */

#define __sub_EXPAND___(x) $ ## x
#define __sub_EXPAND__(x) __sub_EXPAND___(x)
//#define __sub_EXPAND__(x) x
#define __sub_EXPAND_(x) __sub_EXPAND__(x)
#define $send(obj,name) ((obj)->self.class_->name)
#define $field(obj,name) ((obj)->self.name)
#define $static(obj,name) __sub_EXPAND_(obj##_##name)
#define $alloc() goo_self_alloc()
#define $number_of_properties(object) ($send(object, display_))->properties

void *goo_dyncast_(goo_object *, const goo_class_witness * witness);

void goo_object_init(goo_object *);
#define $goo_object_init(self) goo_object_init($as(self,goo_object))

void goo_object_destroy(goo_object *);
#define $goo_object_destroy(self) goo_object_destroy($as(self,goo_object))

/* Safe casting operations
 * ============================
 *
 * $as(object, class) is `object` upcasted to `class` if this is safe, or a
 * compilation error otherwise. (This is like C++ static_cast<...>).
 *
 * $cast(object, class) is `object` casted to `class` if it is an instance of
 * `class`, or NULL otherwise. (This is like C++ dynamic_cast<...>)
 *
 * $when(class, var, obj) { body } binds obj casted to `class` in body if this
 * is safe, or skip the body otherwise.
 *
 * $when(b, instance_of_b, my_object)
 * {
 *   x += $send(instance_of_b, method_of_b)(instance_of_b, 42);
 * }
 */

#define $as(obj, type) (goo_##type##_inst_(&(obj)->type))

#define $cast(obj, type) \
  ((type*)goo_dyncast_($as(obj, goo_object), &(goo_##type##_witness)))

#define $when(type, var, obj) \
  for (type *var = $cast(obj, type); var != NULL; var = NULL)

/* Collections */

#define goo_assert(x) do { if (!(x)) { fprintf(stderr, "%s failed\n", #x); abort(); }; } while(0)

typedef struct goo_port goo_port;

struct goo_port {
  goo_object *parent;
  void (* disconnect)(goo_object *object);
  goo_object *prev, *next;
};

typedef struct goo_collection goo_collection;

struct goo_collection {
  goo_object *first;
  goo_object *last;
};

#define GOO_PORT(target, target_field, source) \
  source *target##_##target_field##_get(target *object); \
  void target##_##target_field##_disconnect(target *object);

#define GOO_INTERNAL_PORT(target, target_field, source)                  \
  source *target##_##target_field##_get(target *object)                  \
  {                                                                      \
    goo_assert (object != NULL);                                         \
    if (!$field(object, target_field).parent) return NULL;               \
    source *result = $cast($field(object, target_field).parent, source); \
    goo_assert (result);                                                 \
    return result;                                                       \
  }                                                                      \
                                                                         \
  void target##_##target_field##_disconnect(target *object)              \
  {                                                                      \
    if ($field(object, target_field).parent)                             \
    {                                                                    \
      goo_assert ($field(object, target_field).disconnect);              \
      $field(object, target_field).disconnect($as(object, goo_object));  \
      $send(object, on_##target_field##_disconnect)(object);             \
    }                                                                    \
    goo_assert (!$field(object, target_field).parent);                   \
    goo_assert (!$field(object, target_field).disconnect);               \
    goo_assert (!$field(object, target_field).prev);                     \
    goo_assert (!$field(object, target_field).next);                     \
  }

#define GOO_COLLECTION_(name, source, target) \
  target *name##prev(target *object);         \
  target *name##next(target *object);         \
  target *name##first(source *self);          \
  target *name##last(source *self);           \
  source *name##parent(target *self)

#define GOO_COLLECTION(source, source_field, target) \
  GOO_COLLECTION_(source##_##source_field##_, source, target)

#define GOO_SLOT(source, source_field, target) \
  target *source##_##source_field##_get(source *self)

#define GOO_COLLECTION_METHODS(source, source_field, target) \
  void (* const on_##source_field##_disconnect)(source* self, target *object)

#define GOO_SLOT_METHODS(source, source_field, target) \
  void (* const on_##source_field##_disconnect)(source* self, target *object)

#define GOO_PORT_METHODS(target, target_field) \
  void (* const on_##target_field##_disconnect)(target* self)

#define GOO_INTERNAL_COLLECTION_(name, source, source_field, source_prop, target, target_field, target_prop) \
  static void name##_unlink(source *self, target *object)                     \
  {                                                                           \
    if ($field(object, target_field).prev == NULL)                            \
      $field(self, source_field).first = $field(object, target_field).next;   \
    else                                                                      \
      $field((target*)$field(object, target_field).prev, target_field).next = \
        $field(object, target_field).next;                                    \
                                                                              \
    if ($field(object, target_field).next == NULL)                            \
      $field(self, source_field).last = $field(object, target_field).prev;    \
    else                                                                      \
      $field((target*)$field(object, target_field).next, target_field).prev = \
        $field(object, target_field).prev;                                    \
  }                                                                           \
                                                                              \
  static void name##_link(source *self, target *object, target *after_that)   \
  {                                                                           \
    $field(object, target_field).prev = (goo_object*)after_that;              \
    $field(object, target_field).next =                                       \
      after_that ? $field(after_that, target_field).next                      \
                 : $field(self, source_field).first;                          \
    if ($field(object, target_field).prev == NULL)                            \
    {                                                                         \
      goo_assert ($field(self, source_field).first ==                         \
                  $field(object, target_field).next);                         \
      $field(self, source_field).first = (goo_object*)object;                 \
    }                                                                         \
    else                                                                      \
    {                                                                         \
      target *prev = $cast($field(object, target_field).prev, target);        \
      goo_assert (prev && $field(prev, target_field).next ==                  \
                          $field(object, target_field).next);                 \
      $field(prev, target_field).next = (goo_object*)object;                  \
    }                                                                         \
    if ($field(object, target_field).next == NULL)                            \
    {                                                                         \
      goo_assert ($field(self, source_field).last ==                          \
                  $field(object, target_field).prev);                         \
      $field(self, source_field).last = (goo_object*)object;                  \
    }                                                                         \
    else                                                                      \
    {                                                                         \
      target *next = $cast($field(object, target_field).next, target);        \
      goo_assert (next && $field(next, target_field).prev ==                  \
                            $field(object, target_field).prev);               \
      $field(next, target_field).prev = (goo_object*)object;                  \
    }                                                                         \
  }                                                                           \
                                                                              \
  static void _self_disconnect_##source_field(target *object)                 \
  {                                                                           \
    source *self = $cast($field(object, target_field).parent, source);        \
    goo_assert (self);                                                        \
                                                                              \
    name##_unlink(self, object);                                              \
                                                                              \
    $field(object, target_field).prev = NULL;                                 \
    $field(object, target_field).next = NULL;                                 \
    $field(object, target_field).parent = NULL;                               \
    $field(object, target_field).disconnect = NULL;                           \
                                                                              \
    $ml_goo_port_disconnect(self, source_prop, object, target_prop,           \
                            $send(self, on_##source_field##_disconnect));     \
  }                                                                           \
                                                                              \
  target *name##prev(target *object)                                          \
  {                                                                           \
    if ($field(object, target_field).prev == NULL) return NULL;               \
    target *result = $cast($field(object, target_field).prev, target);        \
    goo_assert (result);                                                      \
    if ($field(result, target_field).disconnect != (void*)_self_disconnect_##source_field) \
      return NULL;                                                            \
    return result;                                                            \
  }                                                                           \
                                                                              \
  target *name##next(target *object)                                          \
  {                                                                           \
    if ($field(object, target_field).next == NULL) return NULL;               \
    target *result = $cast($field(object, target_field).next, target);        \
    goo_assert (result);                                                      \
    if ($field(result, target_field).disconnect != (void*)_self_disconnect_##source_field) \
      return NULL;                                                            \
    return result;                                                            \
  }                                                                           \
                                                                              \
  target *name##first(source *self)                                           \
  {                                                                           \
    if ($field(self, source_field).first == NULL) return NULL;                \
    target *result = $cast($field(self, source_field).first, target);         \
    goo_assert (result);                                                      \
    return result;                                                            \
  }                                                                           \
                                                                              \
  target *name##last(source *self)                                            \
  {                                                                           \
    if ($field(self, source_field).last == NULL) return NULL;                 \
    target *result = $cast($field(self, source_field).last, target);          \
    goo_assert (result);                                                      \
    return result;                                                            \
  }                                                                           \
                                                                              \
  source *name##parent(target *self)                                          \
  {                                                                           \
    if ($field(self, target_field).disconnect != (void*)_self_disconnect_##source_field) \
      return NULL;                                                            \
    source *result = $cast($field(self, target_field).parent, source);        \
    goo_assert (result);                                                      \
    return result;                                                            \
  }                                                                           \
                                                                              \
  static void _connect_##source_field(source *self, target *that, target *after_that) \
  {                                                                           \
    goo_assert (self != NULL && that != NULL);                                \
    /* Incorrect use: put after a child belonging to another container. */    \
    if (after_that)                                                           \
      goo_assert($field(after_that, target_field).parent == (void*)self);     \
    if ($field(that, target_field).parent == (goo_object*)self &&             \
        $field(that, target_field).disconnect == (void*)_self_disconnect_##source_field) \
    { /* Reorder case */                                                      \
      target *prev = $cast($field(that, target_field).prev, target);          \
      goo_assert (prev == (void*)$field(that, target_field).prev);            \
      if (after_that == prev) return;                                         \
      name##_unlink(self, that);                                              \
      name##_link(self, that, after_that);                                    \
    }                                                                         \
    else                                                                      \
    { /* New connection / move to new collection case */                      \
      target##_##target_field##_disconnect(that);                             \
      $ml_goo_port_connect(self, source_prop, that, target_prop);             \
      $field(that, target_field).parent = (goo_object*)self;                  \
      $field(that, target_field).disconnect = (void*)_self_disconnect_##source_field;    \
      name##_link(self, that, after_that);                                    \
    }                                                                         \
  }

#define GOO_INTERNAL_COLLECTION(source, source_field, source_prop, target, target_field, target_prop) \
  GOO_INTERNAL_COLLECTION_(source##_##source_field##_, \
      source, source_field, source_prop, target, target_field, target_prop)

#define GOO_INTERNAL_SLOT(source, source_field, source_prop, target, target_field, target_prop) \
  static void _self_disconnect_##source_field(target *object)             \
  {                                                                       \
    source *self = $cast($field(object, target_field).parent, source);    \
    goo_assert (self);                                                    \
                                                                          \
    goo_assert ($field(self, source_field) == object);                    \
    $field(self, source_field) = NULL;                                    \
    $field(object, target_field).prev = NULL;                             \
    $field(object, target_field).next = NULL;                             \
    $field(object, target_field).parent = NULL;                           \
    $field(object, target_field).disconnect = NULL;                       \
                                                                          \
    $ml_goo_port_disconnect(self, source_prop, object, target_prop,       \
                            $send(self, on_##source_field##_disconnect)); \
  }                                                                       \
                                                                          \
  target *source##_##source_field##_get(source *self)                     \
  {                                                                       \
    target *result = $field(self, source_field);                          \
    if (result == NULL) return NULL;                                      \
    goo_assert ($field(result, target_field).disconnect ==                \
                (void*)_self_disconnect_##source_field);                  \
    return result;                                                        \
  }                                                                       \
                                                                          \
  static void $connect_##source_field(source *self, target *that)         \
  {                                                                       \
    goo_assert (self != NULL && that != NULL);                            \
                                                                          \
    if ($field(that, target_field).parent == (goo_object*)self &&         \
        $field(that, target_field).disconnect ==                          \
               (void*)_self_disconnect_##source_field)                    \
      return;                                                             \
                                                                          \
    if ($field(self, source_field))                                       \
      target##_##target_field##_disconnect($field(self, source_field));   \
                                                                          \
    if ($field(that, target_field).parent)                                \
      target##_##target_field##_disconnect(that);                         \
                                                                          \
    $ml_goo_port_connect(self, source_prop, that, target_prop);           \
    $field(that, target_field).parent = (goo_object*)self;                \
    $field(that, target_field).disconnect =                               \
      (void*)_self_disconnect_##source_field;                             \
    $field(self, source_field) = that;                                    \
  }

#define GOO_INTERNAL_TABLE_COLLECTION(source_field) \
  GOO_INTERNAL_TABLE_METHOD(on_##source_field##_disconnect)

#define GOO_INTERNAL_TABLE_SLOT(source_field) \
  GOO_INTERNAL_TABLE_METHOD(on_##source_field##_disconnect)

#define GOO_INTERNAL_TABLE_PORT(source_field) \
  GOO_INTERNAL_TABLE_METHOD(on_##source_field##_disconnect)

#endif /* !__GOO_SYSTEM_H__ */
