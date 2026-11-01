/*
** Syml:
**  Startup and entry point
*/
#define SYML_CORE
#include <syml/def.h>
#include <syml/mem.h>
#include <syml/err.h>

extern void syml_main(void) __asm__("syml.main");

EXPORT int main() {
  syml_main();
  return 0;
}