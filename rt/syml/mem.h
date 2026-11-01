/*
** Syml:
**  internal memory management functions
*/
#ifndef MEM_H
#define MEM_H
#include <syml/def.h>
#include <syml/err.h>
#include <stddef.h>

void *mem_alloc(size_t);
void *mem_realloc(void *, size_t, size_t);
void mem_free(void *);
#endif