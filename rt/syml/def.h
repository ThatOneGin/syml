/*
** Syml:
**  definitions used across this library
*/
#ifndef DEF_H
#define DEF_H
#include <stdint.h>
#include <stdio.h>

#if defined(__GNUC__) || defined(__clang__)
# define NORET __attribute__((noreturn))
# define INLINE __always_inline static inline
# define EXPORT __attribute__((visibility("default")))
#elif defined(_MSC_VER) && (defined(_WIN32) || defined(_WIN64))
# define NORET __declspec(noreturn)
# define INLINE __forceinline static inline
# define EXPORT __declpsec(dllexport)
#else
# define NORET
# define INLINE static inline
# define EXPORT
#endif

#if !defined(SYML_CORE)
# undef EXPORT
# define EXPORT extern
#endif

#endif