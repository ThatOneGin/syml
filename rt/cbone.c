#define CBONE_IMPL
#include "../cbone.h"

#define FILE(x) {x ".c", x ".o"}

struct {
  char *src, *obj;
} files[] = {
  FILE("startup"),
  FILE("err"),
  FILE("mem")
};

void build_c(cbone_cmd *cmd) {
  cbone_cmd_append(cmd, cc);
  cbone_cmd_append(cmd, "-Wall");
  cbone_cmd_append(cmd, "-Wextra");
  cbone_cmd_append(cmd, "-fvisibility=hidden");
  cbone_cmd_append(cmd, "-I");
  cbone_cmd_append(cmd, ".");
  cbone_cmd_append(cmd, "-c");
  cbone_cmd_append(cmd, "-fPIC");
  int n = sizeof(files) / sizeof(files[0]);
  for (int i = 0; i < n; i++) {
    cbone_cmd_append(cmd, files[i].src);
    cbone_cmd_run_sync(cmd);
    cmd->data.size--;
  }
  cmd->data.size = 0;
}

void build_lib(cbone_cmd *cmd) {
  cbone_cmd_append(cmd, "gcc");
  cbone_cmd_append(cmd, "-o");
  cbone_cmd_append(cmd, "libsyml.so");
  cbone_cmd_append(cmd, "-fvisibility=hidden");
  cbone_cmd_append(cmd, "-shared");
  int n = sizeof(files) / sizeof(files[0]);
  for (int i = 0; i < n; i++) {
    cbone_cmd_append(cmd, files[i].obj);
  }
  cbone_cmd_run_async_reset(cmd);
}

void clean(cbone_cmd *cmd) {
  cbone_cmd_append(cmd, "rm");
  int n = sizeof(files) / sizeof(files[0]);
  for (int i = 0; i < n; i++) {
    cbone_cmd_append(cmd, files[i].obj);
  }
  cbone_cmd_append(cmd, "libsyml.so");
  cbone_cmd_run_async_reset(cmd);
}

int main(int argc, char **argv) {
  char *build_cmd;
  cbone_cmd cmd;
  if (argc == 1) {
    build_c(&cmd);
    build_lib(&cmd);
    goto ret;
  } else build_cmd = argv[1];
  if (strcmp(build_cmd, "build") == 0) {
    build_c(&cmd);
    build_lib(&cmd);
  } else if (strcmp(build_cmd, "clean") == 0) {
    clean(&cmd);
  } else
    cbone_log(NULL, "unknown command %s\n", build_cmd);
ret:
  cbone_cmd_free(&cmd);
  return 0;
}
