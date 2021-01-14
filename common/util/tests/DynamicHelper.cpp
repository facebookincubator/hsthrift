// Copyright (c) Facebook, Inc. and its affiliates.

#include "tests/DynamicHelper.h"

#include <fstream>

const folly::dynamic* newDynamic() noexcept {
  folly::dynamic* d = new folly::dynamic(folly::dynamic::object);

  d->insert("int", 42);
  d->insert("string", "wibble");
  d->insert("double", 1000.0 / 1024.0);
  d->insert("array", folly::dynamic::array(1, 2, 3));
  d->insert("null", nullptr);
  d->insert("object", folly::dynamic::object("a", "b"));
  d->insert("bool", true);

  return d;
}

const HsJSON* newHsJSON() noexcept {
  std::unique_ptr<const folly::dynamic> d(newDynamic());
  return new HsJSON(*d);
}

static std::string buf;

void initializeJson(const char* filename) noexcept {
  std::ifstream fin(filename);
  buf.assign(
      std::istreambuf_iterator<char>(fin), std::istreambuf_iterator<char>());
}

const char* getJsonAsCStringLen(int64_t* len) noexcept {
  *len = buf.size();
  return buf.data();
}

const folly::dynamic* getJsonAsPtrDynamic() noexcept {
  auto d = parseJson(folly::StringPiece(buf.data(), buf.size()));
  return new folly::dynamic(std::move(d));
}

const HsJSON* getJsonAsPtrHsJSON() noexcept {
  auto d = parseJson(folly::StringPiece(buf.data(), buf.size()));
  return new HsJSON(d);
}

const folly::dynamic* parseJSON(const char* str, int64_t len) noexcept {
  auto d = parseJson(folly::StringPiece(str, len));
  return new folly::dynamic(std::move(d));
}

int64_t compareDynamic(
    const folly::dynamic* lhs,
    const folly::dynamic* rhs) noexcept {
  return *lhs == *rhs ? 1 : 0;
}
