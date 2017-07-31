#ifndef __GOO_TYPES_H__
#define __GOO_TYPES_H__

#include <string.h>

typedef int goo_bool;

/* Goo string: an abstraction over fixed size strings.
 * Designed to ease operation with OCaml GC that can move values.  */
typedef struct {
  const char *(*data)(void *value);
  size_t (*length)(void *value);
  void (*release)(void *value);
} goo_string_class;

typedef struct {
  void *value;
  goo_string_class *table;
} goo_string;

const char *goo_string_data(goo_string str);
int goo_string_length(goo_string str);
void goo_string_release(goo_string str);

goo_string null_string;

goo_string goo_string_from_c(const char *string, goo_bool free);

#endif /* !__GOO_TYPES_H__ */
