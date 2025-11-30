/*
MIT License

Copyright (c) 2025 ThatOneGin

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#ifndef CBONE_H
#define CBONE_H
#ifdef __linux__
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <dirent.h>
typedef int cbone_fd;
#define path_sep "/"
#define CBONE_FD_INVALID -1
#elif defined(_WIN32)
#include <process.h>
#include <direct.h>
#include <windows.h>
typedef HANDLE cbone_fd;
#define path_sep "\\"
#define CBONE_FD_INVALID INVALID_HANDLE_VALUE
#endif

#ifdef __clang__
  #define cc "clang"
#elif defined(__GNUC__)
  #define cc "gcc"
#elif defined(__MSC_VER)
  #define cc "cl.exe"
#else
  #define cc "cc"
#endif

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>

typedef struct {
  char **items;
  size_t size;
  size_t capacity;
} cbone_str_array;

typedef struct {
  cbone_str_array data;
} cbone_cmd;

typedef struct {
  char *items;
  size_t size;
  size_t capacity;
} cbone_string_builder;

/*
** variable to identify if any non-critical errors happened.
*/
extern int cbone_errcode;
int cbone_cmd_append(cbone_cmd *cmd, char *s);
void cbone_cmd_free(cbone_cmd *cmd);
cbone_fd cbone_cmd_run_async(cbone_cmd *cmd);
cbone_fd cbone_cmd_run_async_reset(cbone_cmd *cmd);
int cbone_fd_wait(cbone_fd f);
cbone_fd cbone_fd_open(char *path);
void cbone_fd_close(cbone_fd f);
int cbone_cmd_run_sync(cbone_cmd *cmd);
int cbone_cmd_run_sync_reset(cbone_cmd *cmd);
int cbone_fd_rename(const char *old_name, const char *new_name);
int cbone_fd_modified_after(char *f1, char *f2);
void cbone_rebuild_self_(int argc, char **argv, char *source_file);
cbone_str_array cbone_make_str_array(char *first, ...);
char *cbone_concat_str_array(char *delim, cbone_str_array s);
char *cbone_str_concat(char *s1, char *s2);
void cbone_assert_with_errmsg(int expr, char *errmsg);
int cbone_dir_exists(cbone_str_array Array_path);
int cbone_dir_mkdir(char *path);
int cbone_dir_rmdir(char *path);
cbone_string_builder cbone_sb_new(void);
int cbone_sb_sprintf(cbone_string_builder *sb, const char *f, ...);
int cbone_sb_char(cbone_string_builder *sb, const char c);
int cbone_sb_int(cbone_string_builder *sb, int i);
size_t cbone_sb_free(cbone_string_builder *sb);
char *cbone_sb_cstr(cbone_string_builder *sb);
void cbone_log(const char *pref, const char *f, ...);
/*
**Dynamic arrays for utilities

CBONE_DA_DEFAULT_CAP: minimum capacity for arrays (customizable)
CBONE_ASSERT: assertion method used in errors

CBONE_DA_FREE: free an dynamic array.

CBONE_DA_PUSH: push an element to the front of an array

CBONE_DA_POP: remove an element on the front of the array.

CBONE_DA_PUSH_AT: push an element at position (adjust others to fit)

CBONE_DA_POP_AT: remove an element at position (adjust others to fill)

CBONE_DA_GET: gets an element at given position, if the position is greater
than the size, it will give the last element. Otherwise if it underflows, the
first.

CBONE_DA_RESERVE: adjust the size of the dynamic array to expected size.
*/

#ifndef CBONE_DA_DEFAULT_CAP
#define CBONE_DA_DEFAULT_CAP 64
#endif

#ifndef CBONE_ASSERT
#include <assert.h>
#define CBONE_ASSERT assert
#endif

#define CBONE_ERRLOG(msg) cbone_log("ERROR", msg)

#define CBONE_DA_FREE(arr)                                                     \
  do {                                                                         \
    if ((arr).capacity > 0) {                                                  \
      free((arr).items);                                                       \
    }                                                                          \
  } while (0)

#define CBONE_DA_PUSH(arr, elm)                                                \
  do {                                                                         \
    CBONE_DA_RESERVE(arr, (arr).size + 1);                                     \
    (arr).items[(arr).size++] = (elm);                                         \
  } while (0)

#define CBONE_DA_POP(arr)                                                    \
  do {                                                                       \
    if ((arr).capacity > 0 && (arr).size > 0) {                              \
      (arr).size--;                                                          \
    }                                                                        \
  } while (0)

#define CBONE_DA_POP_AT(arr, pos)                                              \
  do {                                                                         \
    if ((pos) < (arr).size) {                                                  \
      for (size_t i = (pos); i < (size_t)(arr).size - 1; i++) {                \
        (arr).items[i] = (arr).items[i + 1];                                   \
      }                                                                        \
      (arr).size--;                                                            \
    }                                                                          \
  } while (0)

#define CBONE_DA_GET(arr, pos) ((pos) >= 0 ? (arr).size > (pos) ? (arr).items[(pos)] : (arr).items[(arr).size - 1] : (arr).items[0])

#define CBONE_DA_PUSH_AT(arr, elm, pos)                                        \
  do {                                                                         \
    if ((arr).size + (pos) < (arr).capacity) {                                 \
      for (size_t i = (arr).size; i > pos; i--) {                              \
        (arr).items[i] = (arr).items[i - 1];                                   \
      }                                                                        \
      (arr).items[pos] = elm;                                                  \
    }                                                                          \
  } while (0)

#define CBONE_DA_RESERVE(arr, new_cap)                                           \
  do {                                                                           \
    if ((arr).capacity < (new_cap)) {                                            \
      if ((arr).capacity == 0) (arr).capacity = CBONE_DA_DEFAULT_CAP;            \
      while ((arr).capacity < new_cap) {                                         \
        (arr).capacity *= 2;                                                     \
      }                                                                          \
      (arr).items = realloc((arr).items, (arr).capacity * sizeof(*(arr).items)); \
      if ((arr).items == NULL) {                                                 \
        CBONE_ERRLOG("CBONE_DA_RESERVE fail: Realloc Error.");                   \
        exit(1);                                                                 \
      }                                                                          \
    }                                                                            \
  } while(0)

/* Implementation section */
#ifdef CBONE_IMPL

#define PATH(...) cbone_concat_str_array(path_sep, cbone_make_str_array(__VA_ARGS__, NULL))
#define CONCAT(...) cbone_concat_str_array("", cbone_make_str_array(__VA_ARGS__, NULL))
#define CMD(...)                                                               \
  do {                                                                         \
    cbone_cmd cmd = {.data = cbone_make_str_array(__VA_ARGS__, NULL)};         \
    cbone_cmd_run_sync(&cmd);                                                  \
    cbone_cmd_free(&cmd);                                                      \
  } while (0)

int cbone_errcode = 0;

char *cbone_str_concat(char *s1, char *s2) {
  char *buffer = malloc(strlen(s1) + strlen(s2) + 1);
  cbone_assert_with_errmsg(buffer != NULL, "Couldn't concat string.");

  strncat(buffer, s1, strlen(s1));
  strncat(buffer, s2, strlen(s2));

  return buffer;
}

void cbone_assert_with_errmsg(int expr, char *errmsg) {
  if (!expr) {
    CBONE_ERRLOG(errmsg);
    exit(1);
  }
}

cbone_str_array cbone_make_str_array(char *first, ...) {
  cbone_str_array result = {0};
  if (first == NULL) {
    return result;
  }
  CBONE_DA_PUSH(result, first);
  va_list ap;
  va_start(ap, first);
  for (char *next = va_arg(ap, char *); next != NULL;
       next = va_arg(ap, char *)) {
    CBONE_DA_PUSH(result, next);
  }
  va_end(ap);
  return result;
}

char *cbone_concat_str_array(char *sep, cbone_str_array s) {
  if (s.size == 0) {
    char *s = malloc(1);
    s[0] = '\0';
    return s;
  }
  cbone_string_builder result = cbone_sb_new();
  for (size_t i = 0; i < s.size; i++) {
    cbone_sb_sprintf(&result, "%s", s.items[i]);
    if (i < s.size - 1) {
      cbone_sb_sprintf(&result, "%s", sep); /* put separator */
    }
  }
  return cbone_sb_cstr(&result);
}

int cbone_cmd_append(cbone_cmd *cmd, char *s) {
  CBONE_DA_PUSH(cmd->data, s);
  return cmd->data.size-1;
}

void cbone_cmd_free(cbone_cmd *cmd) {
  CBONE_DA_FREE(cmd->data);
}

cbone_fd cbone_cmd_run_async(cbone_cmd *cmd) {
  char *str_cmd = cbone_concat_str_array(" ", cmd->data);
  cbone_log("CMD", "%s", str_cmd);
#if defined(__linux) || defined(__linux__)
  free(str_cmd); /* here we just log it */

  pid_t pid = fork();
  if (pid < 0) {
    perror("fork");
    return CBONE_FD_INVALID;
  } else if (pid == 0) {
    CBONE_DA_PUSH(cmd->data, NULL);
    if (execvp(cmd->data.items[0], (char *const *)cmd->data.items) < 0) {
      cbone_log(NULL, "Couldn't execute child process %s: %s", cmd->data.items[0], strerror(errno));
    }
    exit(EXIT_FAILURE);
  }
  return pid;
#elif defined(_WIN32)
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  ZeroMemory(&si, sizeof(si));
  ZeroMemory(&pi, sizeof(pi));
  
  si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
  si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
  si.dwFlags |= STARTF_USESTDHANDLES;
  si.cb = sizeof(si);

  if (CreateProcess(NULL, str_cmd, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi)) {
    WaitForSingleObject(pi.hProcess, INFINITE);
    CloseHandle(pi.hThread);
  } else {
    cbone_log(NULL, "Couldn't execute child process: %ld", GetLastError());
    cbone_errcode = 1;
  }
  free(str_cmd);
  return pi.hProcess;
#endif
}

cbone_fd cbone_cmd_run_async_reset(cbone_cmd *cmd) {
  cbone_fd pid = cbone_cmd_run_async(cmd);
  cmd->data.size = 0;
  return pid;
}

int cbone_fd_wait(cbone_fd f) {
#if defined(__linux) || defined(__linux__)
  int status;

  if (waitpid(f, &status, 0) == -1) {
    cbone_log(NULL, "Couldn't wait for child process: %s", strerror(errno));
    exit(EXIT_FAILURE);
  }
  if (WIFEXITED(status)) {
    int estatus = WEXITSTATUS(status);
    if (estatus != 0) {
      cbone_log(NULL, "Command exited with exit code %s.", estatus);
      return false;
    }
  }
  if (WIFSIGNALED(status)) {
    cbone_log(NULL, "Command was terminated by signal %d.", WTERMSIG(status));
    return false;
  }
  return true;
#elif defined(_WIN32)
  DWORD estatus;
  DWORD status = WaitForSingleObject(f, INFINITE);

  if (status == WAIT_FAILED) {
    cbone_log(NULL, "Couldn't wait child process: %ld", GetLastError());
    return false;
  }
  if (!GetExitCodeProcess(f, &estatus)) {
    cbone_log(NULL, "Couldn't get child process exit code: %ld", GetLastError());
    return false;
  }
  if (estatus != 0) {
    cbone_log(NULL, "Command exited with code %lu.", estatus);
    return false;
  }
  CloseHandle(f);
  return true;
#endif
}

cbone_fd cbone_fd_open(char *path) {
#if defined(__linux) || defined(__linux__)
  cbone_fd fl = open(path, O_RDONLY);

  if (fl < 0) {
    cbone_log("Couldn't open %s (%s)", path, strerror(errno));
    exit(1);
  }
#elif defined(_WIN32)
  cbone_fd fl = CreateFile(path, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING,
                     FILE_ATTRIBUTE_NORMAL, NULL);

  if (fl == INVALID_HANDLE_VALUE) {
    cbone_log(NULL, "Couldn't open %s (%ld)", path, GetLastError());
    exit(1);
  }
#endif
  return fl;
}

void cbone_fd_close(cbone_fd f) {
#ifdef _WIN32
  CloseHandle(f);
#elif defined(__linux) || defined(__linux__)
  close(f);
#endif
}

int cbone_cmd_run_sync(cbone_cmd *cmd) {
  cbone_fd f = cbone_cmd_run_async(cmd);
  return cbone_fd_wait(f);
}

int cbone_cmd_run_sync_reset(cbone_cmd *cmd) {
  int status = cbone_cmd_run_sync(cmd);
  cmd->data.size = 0;
  return status;
}

int cbone_fd_modified_after(char *f1, char *f2) {
#ifdef _WIN32
  FILETIME file1_time, file2_time;
  cbone_fd file1 = cbone_fd_open(f1);
  cbone_fd file2 = cbone_fd_open(f2);

  if (!GetFileTime(file1, NULL, NULL, &file1_time)) {
    cbone_log("Couldn't get time of %s (%ld)", f1, GetLastError());
    cbone_errcode = 1;
    return 0;
  }
  cbone_fd_close(file1);

  if (!GetFileTime(file2, NULL, NULL, &file2_time)) {
    cbone_log("Couldn't get time of %s (%ld)", f2, GetLastError());
    cbone_errcode = 1;
    return 0;
  }
  cbone_fd_close(file2);

  return CompareFileTime(&file1_time, &file2_time) == 1;
#elif defined(__linux) || defined(__linux__)
  struct stat flstat_buffer = {0};

  if (stat(f1, &flstat_buffer) < 0) {
    perror("Couldn't get file time");
    cbone_errcode = 1;
    return 0;
  }

  int f1_time = flstat_buffer.st_mtime;

  if (stat(f2, &flstat_buffer) < 0) {
    perror("Couldn't get file time");
    cbone_errcode = 1;
    return 0;
  }

  int f2_time = flstat_buffer.st_mtime;

  return f1_time > f2_time;
#endif
}

int cbone_fd_rename(const char *old_name, const char *new_name) {
  cbone_log(NULL, "renaming %s to %s.", old_name, new_name);
  if (rename(old_name, new_name) < 0) {
    perror("Couldn't rename file");
    return 0;
  }
  return 1;
}

void cbone_rebuild_self_(int argc, char **argv, char *source_file) {
  cbone_assert_with_errmsg(argc >= 1, "no args... how?");
  char *target = argv[0];
  if (cbone_fd_modified_after(source_file, target)) {
    /* got this idea from nob.h, ref: https://github.com/tsoding/nob.h */
#if !defined(CBONE_DISABLE_BACKUP_FILE)
    cbone_string_builder sb = cbone_sb_new();
    cbone_sb_sprintf(&sb, "%s.old", target);
    cbone_fd_rename(target,
        cbone_sb_cstr(&sb));
    cbone_sb_free(&sb);
#endif
    CMD(cc, "-o", target, source_file);
    /* rerun the binary */
    cbone_cmd cmd = {0};
    cbone_cmd_append(&cmd, target);
    cbone_cmd_run_sync(&cmd);
    cbone_cmd_free(&cmd);
    exit(0);
  }
}

#define cbone_rebuild_self(argc, argv) cbone_rebuild_self_(argc, argv, __FILE__)

/*
Returns if given path as an array of strings is a directory.
Return values:
  0: doesn't exists.
  1: exists.
  2: not a directory, but exists.*/
int cbone_dir_exists(cbone_str_array Array_path) {
  char *path = cbone_concat_str_array(path_sep, Array_path);
  int result;
#ifdef _WIN32
  DWORD attr = GetFileAttributes(path);
  if (attr == INVALID_FILE_ATTRIBUTES) {
    /* doesn't exists */
    result = 0;
  } else if (attr & FILE_ATTRIBUTE_DIRECTORY) {
    /* is a directory */
    result = 1;
  } else {
    /* is a file */
    result = 2;
  }
#elif defined(__linux) || defined(__linux__)
  struct stat statbuf;
  if (stat(path, &statbuf) == 0) {
    if (S_ISDIR(statbuf.st_mode)) {
      /* is a directory */
      result = 1;
    } else {
      /* is a file */
      result = 2;
    }
  } else {
    /* doesn't exists */
    result = 0;
  }
#endif
  free(path);
  return result;
}

/* makes a folder with given path in form of a string. */
int cbone_dir_mkdir(char *path) {
  int result;

#ifdef _WIN32
  result = _mkdir(path);
#elif defined(__linux) || defined(__linux__)
  result = mkdir(path, 0755);
#endif
  return result;
}

/* this function only delete empty directories */
/* so if using to delete a folder with files, you should */
/* delete all files in that folder */
int cbone_dir_rmdir(char *path) {
  #ifdef _WIN32
    return RemoveDirectory(path);
  #elif defined(__linux) || defined(__linux__)
    return rmdir(path) == 0;
  #endif
}

cbone_string_builder cbone_sb_new(void) {
  cbone_string_builder sb;
  sb.items = NULL;
  sb.size = 0;
  sb.capacity = 0;
  return sb;
}

int cbone_sb_sprintf(cbone_string_builder *sb, const char *f, ...) {
  va_list ap;
  /* get how much chars we need to reserve */
  va_start(ap, f);
  int n = vsnprintf(NULL, 0, f, ap);
  va_end(ap);
  CBONE_DA_RESERVE(*sb, sb->size + n + 1);
  va_start(ap, f);
  n = vsnprintf(sb->items + sb->size, n+1, f, ap);
  va_end(ap);
  sb->size += n;
  return n;
}

int cbone_sb_char(cbone_string_builder *sb, const char c) {
  return cbone_sb_sprintf(sb, "%c", c);
}

int cbone_sb_int(cbone_string_builder *sb, int i) {
  return cbone_sb_sprintf(sb, "%d", i);
}

size_t cbone_sb_free(cbone_string_builder *sb) {
  size_t nbytes = sb->size;
  CBONE_DA_FREE(*sb);
  return nbytes;
}

char *cbone_sb_cstr(cbone_string_builder *sb) {
  return sb->items;
}

void cbone_log(const char *pref, const char *f, ...) {
  if (pref == NULL) pref = "LOG";
  va_list ap;
  va_start(ap, f);
  fprintf(stdout, "[%s] ", pref);
  vfprintf(stdout, f, ap);
  fprintf(stdout, "\n");
  va_end(ap);
}

/*
** Iterate through a directory ignoring
** '.' and '..'. Also declares a special variable
** 'filename' which is the current file's name.
*/
#if defined(WIN32) || defined(_WIN32)
#define cbone_foreach_file_in(__dir, body)                             \
  {WIN32_FIND_DATA findFileData;                                       \
    HANDLE dir = INVALID_HANDLE_VALUE;                                 \
    char search_path[1024];                                            \
    snprintf(search_path, 1024, "%s\\*", __dir);                       \
    dir = FindFirstFile(search_path, &findFileData);                   \
    CBONE_ASSERT(dir != INVALID_HANDLE_VALUE);                         \
    do {                                                               \
      const char * const filename =                                    \
        findFileData.cFileName;                                        \
      if (strcmp(filename, ".") != 0 && strcmp(filename, "..") != 0) { \
        body;                                                          \
      }                                                                \
    } while (FindNextFile(dir, &findFileData) != 0);                   \
    FindClose(dir);}
#elif defined(__linux__) || defined(__linux)
#define cbone_foreach_file_in(__dir, body)         \
  {DIR *d;                                         \
    struct dirent *dir;                            \
    d = opendir(__dir);                            \
    CBONE_ASSERT(d != NULL);                          \
    while ((dir = readdir(d)) != NULL) {           \
      const char * const filename = dir->d_name;   \
      if (strcmp(filename, ".") != 0 &&            \
          strcmp(filename, "..") != 0) {           \
        body;                                      \
      }                                            \
    }                                              \
    closedir(d);}
#endif

#endif // CBONE_IMPL
#endif // CBONE_H

/*
** got this idea from nob.h library
** ref: https://github.com/tsoding/nob.h
*/
#ifndef CBONE_STRIP_GUARD
#define CBONE_STRIP_GUARD
  #ifdef CBONE_STRIP_PREFIX
    #define rebuild_self cbone_rebuild_self
    #define cmd_append cbone_cmd_append
    #define cmd_free cbone_cmd_free
    #define cmd_run_async cbone_cmd_run_async
    #define cmd_run_async_reset cbone_cmd_run_async_reset
    #define fd_wait cbone_fd_wait
    #define fd_open cbone_fd_open
    #define fd_close cbone_fd_close
    #define cmd_run_sync cbone_cmd_run_sync
    #define cmd_run_sync_reset cbone_cmd_run_sync_reset
    #define fd_rename cbone_fd_rename
    #define fd_modified_after cbone_fd_modified_after
    #define make_str_array cbone_make_str_array
    #define concat_str_array cbone_concat_str_array
    #define str_concat cbone_str_concat
    #define assert_with_errmsg cbone_assert_with_errmsg
    #define dir_exists cbone_dir_exists
    #define dir_mkdir cbone_dir_mkdir
    #define dir_rmdir cbone_dir_rmdir
    #define sb_new cbone_sb_new
    #define sb_sprintf cbone_sb_sprintf
    #define sb_char cbone_sb_char
    #define sb_int cbone_sb_int
    #define sb_free cbone_sb_free
    #define sb_cstr cbone_sb_cstr
    #define DA_FREE CBONE_DA_FREE
    #define DA_PUSH CBONE_DA_PUSH
    #define DA_POP CBONE_DA_POP
    #define DA_PUSH_AT CBONE_DA_PUSH_AT
    #define DA_POP_AT CBONE_DA_POP_AT
    #define DA_GET CBONE_DA_GET
    // already defined in math.h
    // #define log cbone_log
  #endif // CBONE_STRIP_PREFIX
#endif // CBONE_STRIP_GUARD
