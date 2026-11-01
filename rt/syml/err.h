/*
** Syml:
**  error functions
*/
#ifndef ERR_H
#define ERR_H
#include <syml/def.h>
#include <stdio.h>

typedef enum {
  err_ok,
  err_run_error,
  err_mem_error,
  err_val_error,
  ecnt,
} errcode_t;

void set_errcode(int);
NORET void bad_alloc(void);
NORET void bad_value(void);
NORET void out_of_bounds(void);
NORET void run_error(const char *);
NORET void run_errorf(const char *, ...);
#endif