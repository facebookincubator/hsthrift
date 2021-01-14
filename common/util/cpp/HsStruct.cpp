// Copyright (c) Facebook, Inc. and its affiliates.

#include "cpp/HsStruct.h"

#include "cpp/Marshallable.h"

/*
 * HsJSON
 */

HsJSON::HsJSON(const folly::dynamic& value) {
  switch (value.type()) {
    case folly::dynamic::NULLT:
      type = Type::Null;
      break;
    case folly::dynamic::BOOL:
      type = Type::Bool;
      integral = value.asBool();
      break;
    case folly::dynamic::INT64:
      type = Type::Integral;
      integral = value.asInt();
      break;
    case folly::dynamic::DOUBLE:
      type = Type::Real;
      real = value.asDouble();
      break;
    case folly::dynamic::STRING:
      type = Type::String;
      new (&string) HsString(value.asString());
      break;
    case folly::dynamic::ARRAY:
      type = Type::Array;
      {
        std::vector<HsJSON> v;
        v.reserve(value.size());
        for (const folly::dynamic& i : value) {
          v.emplace_back(i);
        }
        new (&array) HsArray<HsJSON>(std::move(v));
      }
      break;
    case folly::dynamic::OBJECT:
      type = Type::Object;
      {
        std::vector<HsString> keys;
        std::vector<HsJSON> values;
        keys.reserve(value.size());
        values.reserve(value.size());
        for (const auto& i : value.items()) {
          keys.emplace_back(i.first.asString());
          values.emplace_back(i.second);
        }
        new (&object) HsObject<HsJSON>(std::move(keys), std::move(values));
      }
      break;
  }
}

void HsJSON::construct(HsJSON&& other) {
  DCHECK(this != &other);
  type = other.type;
  switch (type) {
    case Type::Null:
      break;
    case Type::Bool:
    case Type::Integral:
      integral = other.integral;
      break;
    case Type::Real:
      real = other.real;
      break;
    case Type::String:
      new (&string) HsString(std::move(other.string));
      break;
    case Type::Array:
      new (&array) HsArray<HsJSON>(std::move(other.array));
      break;
    case Type::Object:
      new (&object) HsObject<HsJSON>(std::move(other.object));
      break;
  }
}

void HsJSON::destruct() {
  if (type == Type::String) {
    string.~HsString();
  } else if (type == Type::Array) {
    array.~HsArray();
  } else if (type == Type::Object) {
    object.~HsMap();
  }
}

/*
 * HsString
 */

HsString* newHsString(const char* p, size_t n) noexcept {
  return new HsString(std::string(p, n));
}

void ctorHsString(HsString* ret, const char* p, size_t n) noexcept {
  new (ret) HsString(std::string(p, n));
}

/*
 * HsEither
 */

// HsEither<char,char> instead of HsEither<void,void> just to keep class
// definition happy.
void* newHsEither(HsEitherTag tag, void* val) {
  return new DummyHsEither(tag, val);
}

HS_DEFINE_MARSHALLABLE(HsMaybeInt, HsMaybe<int64_t>);
HS_DEFINE_MARSHALLABLE(HsMaybeDouble, HsMaybe<double>);
HS_DEFINE_MARSHALLABLE(HsMaybeString, HsMaybe<HsString>);

HS_DEFINE_MARSHALLABLE(HsEitherStringInt, HsEither<HsString, int64_t>);
HS_DEFINE_MARSHALLABLE(HsEitherStringDouble, HsEither<HsString, double>);
HS_DEFINE_MARSHALLABLE(HsEitherStringString, HsEither<HsString, HsString>);

HS_DEFINE_MARSHALLABLE(
    HsEitherStringArrayInt,
    HsEither<HsString, HsArray<int64_t>>);
HS_DEFINE_MARSHALLABLE(
    HsEitherStringArrayDouble,
    HsEither<HsString, HsArray<double>>);
HS_DEFINE_MARSHALLABLE(
    HsEitherStringArrayString,
    HsEither<HsString, HsArray<HsString>>);

HS_DEFINE_MARSHALLABLE(HsString, HsString);

HS_DEFINE_MARSHALLABLE(HsArrayInt, HsArray<int64_t>);
HS_DEFINE_MARSHALLABLE(HsArrayDouble, HsArray<double>);
HS_DEFINE_MARSHALLABLE(HsArrayString, HsArray<HsString>);

HS_DEFINE_MARSHALLABLE(HsMapIntInt, HsMap<int64_t, int64_t>);
HS_DEFINE_MARSHALLABLE(HsMapIntDouble, HsMap<int64_t, double>);
HS_DEFINE_MARSHALLABLE(HsMapIntString, HsMap<int64_t, HsString>);
HS_DEFINE_MARSHALLABLE(HsMapStringInt, HsMap<HsString, int64_t>);
HS_DEFINE_MARSHALLABLE(HsMapStringDouble, HsMap<HsString, double>);
HS_DEFINE_MARSHALLABLE(HsMapStringString, HsMap<HsString, HsString>);

HS_DEFINE_MARSHALLABLE(HsJSON, HsJSON);
