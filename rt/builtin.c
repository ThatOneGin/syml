/*
** Syml:
**  builtin functions
**   some of these are not meant to be used
**   directly, but by wrappers
*/
#define SYML_CORE
#include <syml/err.h>
#include <syml/mem.h>
#include <syml/def.h>

/* do not change field orders */
typedef struct {
  int64_t size;
  int64_t align;
  char *name;
} syml_typedesc;

EXPORT void *__syml_alloc(syml_typedesc *T) {return mem_alloc(T->size);}
EXPORT void __syml_free(void *p) {mem_free(p);}

/* these are all acessible through a typeinfo expression */

EXPORT int64_t __syml_sizeof(syml_typedesc *T) {return T->size;}
EXPORT int64_t __syml_alignof(syml_typedesc *T) {return T->align;}
EXPORT char *__syml_typename(syml_typedesc *T) {return T->name;}