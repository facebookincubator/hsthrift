// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

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
