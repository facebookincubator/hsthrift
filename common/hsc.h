#pragma once

#include <HsFFI.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Utilities for use with hsc2hs.
 */

/*
 * #{ alignment T }
 *
 * Produces the minimal alignment of a data type.
 * NOTE: we can't use the macro that comes with hsc2hs, because
 * it doesn't build clearly under clang. TODO(14913295): upstream this
 */
#undef hsc_alignment
#define hsc_alignment(x...)                                           \
  do {                                                                \
    struct __anon_x__ {                                               \
      char a;                                                         \
      x b;                                                            \
    };                                                                \
    hsc_printf("%lu", (unsigned long)offsetof(struct __anon_x__, b)); \
  } while (0)

/*
 * #{ verbatim C++ code }
 *
 * Inserts any C++ code as it is in the code generator. An example use
 * case can be: #{ verbatim using namespace facebook::nodeapi; }
 */
#undef hsc_verbatim
#define hsc_verbatim(...) __VA_ARGS__

extern "C" HsPtr itaniumMangle(HsPtr a1, size_t l);

/*
 * #{ mangled signature }
 *
 * Takes a C++ function signature and mangles it into the corresponding
 * C++ linker symbol.
 *
 *     #{ mangled int foo(const char*) }
 *
 *     "3fooPKc"
 *
 * This lets you write C++ foreign imports more cleanly.
 *
 *     foreign import ccall #{ mangled int foo(const char*) }
 *       foo :: CString -> IO CInt
 *
 * In order to use this plugin, you must link with '@/common/hs:hsc'.
 *
 */
#define hsc_mangled_common(str, x...)                          \
  do {                                                         \
    hs_init(NULL, NULL);                                       \
    char symbol[] = #x;                                        \
    const char* const mangled = reinterpret_cast<const char*>( \
        itaniumMangle(symbol, sizeof(symbol) - 1));            \
    hsc_printf(str, mangled);                                  \
    free(const_cast<char*>(mangled));                          \
  } while (0)

#define hsc_mangled(x...) hsc_mangled_common("\"%s\"", x)

#define hsc_mangled_reference(x...) hsc_mangled_common("\"&%s\"", x)

#define hsc_print_args(n)       \
  for (int i = 0; i < n; i++) { \
    hsc_printf("x%d ", i);      \
  }

#define hsc_check_unsafe_wrapper(f, n, sig)          \
  hsc_printf("%s_ %s\n", #f, #sig);                  \
  hsc_printf("%s %s\n", #f, #sig);                   \
  hsc_printf("%s ", #f);                             \
  hsc_print_args(n);                                 \
  hsc_printf("= checkUnsafe \"%s\" $ %s_ ", #f, #f); \
  hsc_print_args(n) hsc_printf("\n");
