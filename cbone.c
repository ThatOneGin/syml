#define CBONE_IMPL
#define CBONE_STRIP_PREFIX
#include "cbone.h"

#define TESTNAME(file) ("test" path_sep file)
#if defined(__linux) || defined(__linux__)
#  define rm "rm"
#elif defined(_WIN32)
#  define rm "rmdir"
#else
#  error "platform 'remove directory' function is unknown. please define it manually."
#endif

static int hasext(const char *s, char *ext)
{
  char *end = strrchr(s, *ext);
  if (end == NULL) {
    return 0;
  } else {
    return strcmp(end, ext) == 0;
  }
  return 0;
}

static int runtest(cbone_cmd *cmd, const char *test, char *filename)
{
  cbone_log(NULL, "Running test '%s'.", test);
  cmd_append(cmd, "dune");
  cmd_append(cmd, "exec");
  cmd_append(cmd, "syml");
  cmd_append(cmd, filename);
  int status = cmd_run_sync(cmd);
  cbone_log(NULL, "Test '%s' passed.", test);
  cmd->data.size = 0;
  return status;
}

static void runtests(void)
{
  /* TODO: run tests with foreach */
  struct {
    char *name;
    char *path;
  } tests[] = {
    {"binop", TESTNAME("binop.syml")},
    {"call", TESTNAME("call.syml")},
    {"inline", TESTNAME("inline.syml")},
    {"cond-value", TESTNAME("cond-value.syml")},
    {"if-cond", TESTNAME("if-cond.syml")},
    {"params", TESTNAME("params.syml")}
  };
  cbone_cmd cmd = {0};
  for (int i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
    /* at failure we just segfault */
    runtest(&cmd, tests[i].name, tests[i].path);
  }
  cbone_cmd_free(&cmd);
}

static void compile_s_files()
{
  char *target = NULL;
  cbone_foreach_file_in("test", {
    if (hasext(filename, ".s")) {
      target = PATH("test", filename);
      CMD(cc, "-c", target);
      free(target);
    }
  });
  printf("\n");
}

int main(int argc, char **argv)
{
  cbone_rebuild_self(argc, argv);
  if (argc > 1 && strcmp(argv[1], "run-tests") == 0) {
    runtests();
  } else if (argc > 1 && strcmp(argv[1], "run-tests-compile") == 0) {
    runtests();
    compile_s_files();
  } else if (argc > 1 && strcmp(argv[1], "clean") == 0) {
    cbone_cmd cmd = {0};
    cmd_append(&cmd, rm);
    cbone_foreach_file_in(".", {
      if (hasext(filename, ".o"))
        cmd_append(&cmd, (char*)filename);
    })
    cmd_run_async(&cmd);
  } else {
    return 1;
  }
  return 0;
}
