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

folly::dynamic HsJSON::toDynamic() && {
  switch (type) {
    case Type::Null:
      return folly::dynamic();
    case Type::Bool:
      return folly::dynamic(static_cast<bool>(integral));
    case Type::Integral:
      return folly::dynamic(integral);
    case Type::Real:
      return folly::dynamic(real);
    case Type::String:
      return folly::dynamic(std::move(string).toStdString());
    case Type::Array: {
      auto res = folly::dynamic::array();
      auto end = std::make_move_iterator(array.end());
      for (auto itr = std::make_move_iterator(array.begin()); itr != end;
           itr++) {
        res.push_back(std::move(*itr).toDynamic());
      }
      return res;
    }
    case Type::Object: {
      folly::dynamic res = folly::dynamic::object;
      auto items = std::move(object).take_items();

      // Manual version of zipping 2 iterators together
      auto key_itr = std::make_move_iterator(items.first.begin());
      auto key_end = std::make_move_iterator(items.first.end());
      auto val_itr = std::make_move_iterator(items.second.begin());
      auto val_end = std::make_move_iterator(items.second.end());

      while (key_itr != key_end && val_itr != val_end) {
        res.insert(
            std::move(*key_itr).toStdString(), std::move(*val_itr).toDynamic());
        key_itr++;
        val_itr++;
      }
      return res;
    }
  }
  folly::assume_unreachable();
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

void ctorHsJSONNull(HsJSON* p) noexcept {
  new (p) HsJSON();
}

void ctorHsJSONBool(HsJSON* p, bool b) noexcept {
  new (p) HsJSON(b);
}

void ctorHsJSONInt(HsJSON* p, int64_t i) noexcept {
  new (p) HsJSON(i);
}

void ctorHsJSONDouble(HsJSON* p, double d) noexcept {
  new (p) HsJSON(d);
}

void ctorHsJSONString(HsJSON* p, HsString* txt) noexcept {
  new (p) HsJSON(std::move(*txt));
}

void ctorHsJSONArray(HsJSON* p, HsArray<HsJSON>* a) noexcept {
  new (p) HsJSON(std::move(*a));
}

void ctorHsJSONObject(HsJSON* p, HsObject<HsJSON>* o) noexcept {
  new (p) HsJSON(std::move(*o));
}

/*
 * HsObject
 */
extern "C" void common_hs_ctorHsObjectJSON(
    HsObject<HsJSON>* p,
    HsArray<HsString>* keys,
    HsArray<HsJSON>* vals) {
  new (p) HsObject<HsJSON>(
      std::move(*keys).toStdVector(), std::move(*vals).toStdVector());
}

/*
 * HsStringPiece
 */

HsStringPiece* newHsStringPiece(const char* p, size_t n) noexcept {
  return new HsStringPiece(p, n);
}

void ctorHsStringPiece(HsRange<char>* ret, const char* p, size_t n) noexcept {
  new (ret) HsStringPiece(p, n);
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
HS_DEFINE_MARSHALLABLE(HsStringPiece, HsStringPiece);

HS_DEFINE_MARSHALLABLE(HsArrayInt32, HsArray<int32_t>);
HS_DEFINE_MARSHALLABLE(HsArrayInt64, HsArray<int64_t>);
HS_DEFINE_MARSHALLABLE(HsArrayUInt8, HsArray<uint8_t>);
HS_DEFINE_MARSHALLABLE(HsArrayUInt32, HsArray<uint32_t>);
HS_DEFINE_MARSHALLABLE(HsArrayUInt64, HsArray<uint64_t>);
HS_DEFINE_MARSHALLABLE(HsArrayFloat, HsArray<float>);
HS_DEFINE_MARSHALLABLE(HsArrayDouble, HsArray<double>);
HS_DEFINE_MARSHALLABLE(HsArrayString, HsArray<HsString>);
HS_DEFINE_MARSHALLABLE(HsArrayJSON, HsArray<HsJSON>);

HS_DEFINE_MARSHALLABLE(HsMapIntInt, HsMap<int64_t, int64_t>);
HS_DEFINE_MARSHALLABLE(HsMapIntDouble, HsMap<int64_t, double>);
HS_DEFINE_MARSHALLABLE(HsMapIntString, HsMap<int64_t, HsString>);
HS_DEFINE_MARSHALLABLE(HsMapStringInt, HsMap<HsString, int64_t>);
HS_DEFINE_MARSHALLABLE(HsMapStringDouble, HsMap<HsString, double>);
HS_DEFINE_MARSHALLABLE(HsMapStringString, HsMap<HsString, HsString>);

HS_DEFINE_MARSHALLABLE(HsObjectJSON, HsObject<HsJSON>);
HS_DEFINE_MARSHALLABLE(HsJSON, HsJSON);

HS_OPTION_CPP(Bool, bool);
HS_OPTION_CPP(Int32, int32_t);
HS_OPTION_CPP(Int64, int64_t);
HS_OPTION_CPP(UInt32, uint32_t);
HS_OPTION_CPP(UInt64, uint64_t);
HS_OPTION_CPP(Float, float);
HS_OPTION_CPP(Double, double);
HS_OPTION_CPP(String, HsString);
HS_OPTION_CPP(StringView, HsStringPiece);
HS_OPTION_CPP(HsJSON, HsJSON);

HS_DEFINE_ARRAY_CONSTRUCTIBLE(Int32, int32_t);
HS_DEFINE_ARRAY_CONSTRUCTIBLE(Int64, int64_t);
// std::vector<bool> doesn't guarantee contiguous memory layout,
// so use HsArray<uint8_t> to store one bool per byte
HS_DEFINE_ARRAY_CONSTRUCTIBLE(UInt8, uint8_t);
HS_DEFINE_ARRAY_CONSTRUCTIBLE(UInt32, uint32_t);
HS_DEFINE_ARRAY_CONSTRUCTIBLE(UInt64, uint64_t);
HS_DEFINE_ARRAY_CONSTRUCTIBLE(Float, float);
HS_DEFINE_ARRAY_CONSTRUCTIBLE(Double, double);
HS_DEFINE_ARRAY_CONSTRUCTIBLE(String, HsString);
HS_DEFINE_ARRAY_CONSTRUCTIBLE(StringView, HsStringPiece);
HS_DEFINE_ARRAY_CONSTRUCTIBLE(HsJSON, HsJSON);
