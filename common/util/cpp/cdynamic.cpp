// Copyright (c) Facebook, Inc. and its affiliates.

#include "cpp/cdynamic.h"

#include <folly/dynamic.h>
#include <folly/json.h>
#include <string.h>
#include "cpp/Destructible.h"

using namespace folly;

namespace facebook {
namespace hs {

/**
 * Read the type and value of a dynamic. If the dynamic is array or object,
 * its size is returned as the value.
 */
void readDynamic(const dynamic* d, DType* ty, DValue* val) noexcept {
  switch (d->type()) {
    case dynamic::STRING:
      *ty = tString;
      val->string = d->c_str();
      break;
    case dynamic::BOOL:
      *ty = tBool;
      val->boolean = d->asBool();
      break;
    case dynamic::DOUBLE:
      *ty = tDouble;
      val->doubl = d->asDouble();
      break;
    case dynamic::INT64:
      *ty = tInt64;
      val->int64 = d->asInt();
      break;
    case dynamic::ARRAY:
      *ty = tArray;
      val->size = d->size();
      break;
    case dynamic::OBJECT:
      *ty = tObject;
      val->size = d->size();
      break;
    case dynamic::NULLT:
      *ty = tNull;
      val->null = nullptr;
      break;
  }
}

/**
 * Read the fields of a dynamic array.  Memory for the array is
 * allocated by the caller.  The size of the array to allocate is
 * returned by readDynamic().
 */
int readDynamicArray(
    const dynamic* d,
    size_t /*size*/,
    const dynamic** elems) noexcept {
  if ((*d).type() != dynamic::ARRAY)
    return 0;

  int i = 0;
  for (const auto& e : *d) {
    elems[i++] = &e;
  }
  return i;
}

/**
 * Read the fields of a dynamic object.  Memory for the arrays are
 * allocated by the caller.  The size of the arrays to allocate is
 * returned by readDynamic().
 */
int readDynamicObject(
    const dynamic* d,
    size_t /*size*/,
    const dynamic** keys,
    const dynamic** vals) noexcept {
  if ((*d).type() != dynamic::OBJECT)
    return 0;

  // Relying on the iterator referring to elements by reference here.
  auto it = (*d).items();
  int i = 0;
  for (auto j = it.begin(); j != it.end(); ++j, ++i) {
    keys[i] = &(j->first);
    vals[i] = &(j->second);
  }
  return i;
}

/**
 * Create a dynamic with nullptr, boolean, double or const char*.
 * It's caller's responsibility to allocate and free the memory.
 */
void createDynamic(dynamic* ret, DType ty, DValue* val) noexcept {
  switch (static_cast<dynamic::Type>(ty)) {
    case dynamic::NULLT:
      new (ret) dynamic(nullptr);
      break;
    case dynamic::BOOL:
      new (ret) dynamic(val->boolean != 0);
      break;
    case dynamic::INT64:
      new (ret) dynamic(val->int64);
      break;
    case dynamic::DOUBLE:
      new (ret) dynamic(val->doubl);
      break;
    case dynamic::STRING:
      new (ret) dynamic(val->string);
      break;
    case dynamic::ARRAY:
      throw std::invalid_argument("call writeDynamicArray for dynamic::ARRAY");
    case dynamic::OBJECT:
      throw std::invalid_argument(
          "call writeDynamicObject for dynamic::OBJECT");
    default:
      __builtin_unreachable();
  }
}

/**
 * Create a dynamic array with given \p elems.
 * It's caller's responsibility to allocate and free the memory for \p ret. The
 * elements of \p elems are invalidated.
 */
void createDynamicArray(dynamic* ret, size_t size, dynamic* elems) noexcept {
  new (ret) dynamic(dynamic::array);
  ret->resize(size);
  for (size_t i = 0; i < size; ++i) {
    ret->at(i) = std::move(elems[i]);
    elems[i].~dynamic();
  }
}

/**
 * Create a dynamic array with given \p keys and \p vals.
 * It's caller's responsibility to allocate and free the memory for \p ret. The
 * elements of \p vals are invalidated.
 */
void createDynamicObject(
    dynamic* ret,
    size_t size,
    const char** keys,
    dynamic* vals) noexcept {
  new (ret) dynamic(dynamic::object);
  for (size_t i = 0; i < size; ++i) {
    ret->insert(keys[i], std::move(vals[i]));
    vals[i].~dynamic();
  }
}

/**
 * parse JSON using folly::parseJson()
 * Returns either
 *  - a pointer to the folly::dynamic representing the parsed JSON. The caller
 *    owns this and is responsible for freeing it.
 *  - nullptr, and *err points to an error message. The caller owns the
 *    memory for the error message, and is responsible for freeing it.
 */
const folly::dynamic*
parseJSON(const char* str, int64_t len, char** err) noexcept {
  try {
    auto d = parseJson(folly::StringPiece(str, len));
    return new folly::dynamic(std::move(d));
  } catch (const std::exception& e) {
    *err = strdup(e.what());
    return nullptr;
  }
}

} // namespace hs
} // namespace facebook

HS_DEFINE_DESTRUCTIBLE(Dynamic, Dynamic)
