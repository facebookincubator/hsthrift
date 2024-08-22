/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <glog/logging.h>
#include <cstdarg>
#include <tuple>

#include "cpp/Destructible.h"
#include "cpp/HsStructDefines.h"

namespace hs_std_tuple::detail {

template <typename T>
T move_one(va_list& args) {
  T* l = va_arg(args, T*);
  return std::move(*l);
}

template <typename... T>
std::tuple<T...> makeTuple(va_list& args) {
  return std::tuple<T...>{move_one<T>(args)...};
}

template <typename Tuple, typename T, size_t I>
void peek_one(Tuple* t, va_list& args) {
  T* l = va_arg(args, T*);

  if constexpr (std::is_nothrow_move_constructible_v<T>) {
    // First delete the default allocated memory
    l->~T();
    // Then in-place move initialize
    new (l) T(static_cast<T&&>(std::move(std::get<I>(*t))));
  } else {
    *l = std::move(std::get<I>(*t));
  }
}

template <typename... T, std::size_t... I>
void peekVals(std::tuple<T...>* t, va_list& args, std::index_sequence<I...>) {
  (peek_one<std::tuple<T...>, T, I>(t, args), ...);
}

} // namespace hs_std_tuple::detail

#define hsc_derive_hs_std_tuple_unsafe(cxx_name...)  \
  hsc_printf(                                        \
      "deriveHsStdTupleUnsafe \"%s\" %lu %lu ",      \
      #cxx_name,                                     \
      (unsigned long)sizeof(hs_std_tuple::cxx_name), \
      (unsigned long)alignof(hs_std_tuple::cxx_name));

#define HS_STD_TUPLE_H(Name, Types) \
  namespace hs_std_tuple {          \
  using Name = HsStdTuple<Types>;   \
  }

/**
 * `__VA_ARGS__` is no good when the types listed have a comma within them.
 * For example, `std::tuple<int, HsEither<int, HsString>>` breaks as
 * `__VA_ARGS__` would give us "int", then "HsEither<int", then "HsString" as
 * the expanded type list. We need to generate code for peeking and poking
 * tuples with the types splatted out in a flat-map. The only way this seemed
 * possible was to (ab)use `va_list` to avoid needing to actually list out the
 * types. Since all the types we're sending along are pointers we don't need to
 * worry about argument promotions.
 */
#define HS_STD_TUPLE_CPP(Name)                                       \
  HS_DEFINE_MARSHALLABLE(Name, hs_std_tuple::Name)                   \
                                                                     \
  extern "C" void std_tuple_poke_##Name(                             \
      hs_std_tuple::Name* ptr, const char* fmt, ...) {               \
    va_list args;                                                    \
    va_start(args, fmt);                                             \
    new (ptr) hs_std_tuple::Name(args);                              \
    va_end(args);                                                    \
  }                                                                  \
                                                                     \
  extern "C" void std_tuple_peek_##Name(                             \
      hs_std_tuple::Name* ptr, const char* fmt, ...) {               \
    va_list args;                                                    \
    va_start(args, fmt);                                             \
    hs_std_tuple::detail::peekVals(                                  \
        ptr->getTuplePtr(),                                          \
        args,                                                        \
        std::make_index_sequence<hs_std_tuple::Name::tuple_size>{}); \
    va_end(args);                                                    \
  }

/**
 * std::tuple<> isn't default constructible, which means we can't derive
 * `Marshallable` for any `std::tuple`. Since we do really want default
 * constructibility, create this intermediate representation that introduces
 * default constructibility with DCHECK support when misused.
 */
template <typename... Ts>
HS_STRUCT HsStdTuple {
  union U {
    char mem_block[sizeof(std::tuple<Ts...>)];
    std::tuple<Ts...> tup;

    U() : mem_block() {}
    ~U() {}
  };

  U data_;

  bool has_tuple_{false};

 public:
  HsStdTuple() : data_(), has_tuple_(false) {}

  explicit HsStdTuple(std::tuple<Ts...> && t) {
    new (&data_.tup) decltype(data_.tup){std::move(t)};
    has_tuple_ = true;
  }

  HsStdTuple(const HsStdTuple&) = delete;
  HsStdTuple& operator=(const HsStdTuple&) = delete;

  HsStdTuple(HsStdTuple && other) noexcept {
    construct(std::move(other));
  }

  HsStdTuple& operator=(HsStdTuple&& other) noexcept {
    if (this != &other) {
      destruct();
      construct(std::move(other));
    }
    return *this;
  }

  explicit HsStdTuple(va_list & args) {
    new (&data_.tup) decltype(data_.tup){
        hs_std_tuple::detail::makeTuple<Ts...>(args)};
    has_tuple_ = true;
  }

  ~HsStdTuple() {
    destruct();
  }

  std::tuple<Ts...>* getTuplePtr() {
    DCHECK(has_tuple_);
    return &data_.tup;
  }

  std::tuple<Ts...> toStdTuple()&& {
    return std::move(data_.tup);
  }

  static constexpr auto tuple_size = std::tuple_size_v<std::tuple<Ts...>>;

 private:
  void construct(HsStdTuple<Ts...> && other) noexcept {
    DCHECK(this != &other);
    has_tuple_ = other.has_tuple_;
    if (has_tuple_) {
      new (&data_.tup) decltype(data_.tup){std::move(other.data_.tup)};
    } else {
      new (&data_.mem_block) decltype(data_.mem_block){
          std::move(*other.data_.mem_block)};
    }
  }

  void destruct() {
    if (has_tuple_) {
      data_.tup.~tuple();
      has_tuple_ = false;
    }
  }
};
