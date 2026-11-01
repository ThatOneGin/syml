/*
** Syml:
**  error functions
*/
#include <syml/err.h>
#include <stdlib.h>
#include <stdarg.h>

static char msg_buf[BUFSIZ] = {0};
static int errcode = err_ok;
static char *msg_prefixes[ecnt] = {
  [err_ok] = "syml",
  [err_run_error] = "runtime error",
  [err_mem_error] = "memory error",
  [err_val_error] = "value error"
};

NORET INLINE void internal_errf(int err, const char *msg)
{
  set_errcode(err);
  char *pref = msg_prefixes[err];
  fputs(pref, stderr);
  fputs(": ", stderr);
  fputs(msg, stderr);
  fflush(stderr);
  exit(EXIT_FAILURE);
}

void set_errcode(int e)
{
  if (e >= ecnt)
    bad_value();
  errcode = e;
}

NORET void bad_alloc(void) {internal_errf(err_mem_error, "bad alloc");}
NORET void bad_value(void) {internal_errf(err_val_error, "bad value");}
NORET void out_of_bounds(void) {internal_errf(err_val_error, "out of bounds");}
NORET void run_error(const char *msg) {internal_errf(err_run_error, msg);}

NORET void run_errorf(const char *f, ...) {
  va_list ap;
  va_start(ap, f);
  vsnprintf(msg_buf, BUFSIZ, f, ap);
  va_end(ap);
  run_error(msg_buf);
}