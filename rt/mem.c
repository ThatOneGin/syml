/*
** Syml:
**  internal memory management functions
*/
#include <syml/def.h>
#include <syml/err.h>
#include <syml/mem.h>
#include <stdlib.h>
#include <stddef.h>

void *mem_alloc(size_t sz)
{
  void *ptr = malloc(sz);
  if (!ptr)
    bad_alloc();
  return ptr;
}

void *mem_realloc(void *ptr, size_t nsz, size_t osz)
{
  (void)osz;
  void *tmp = realloc(ptr, nsz);
  if (!tmp && nsz > 0)
    bad_alloc();
  return tmp;
}

void mem_free(void *ptr)
{
  if (ptr) {
    free(ptr);
    ptr = NULL;
  }
}