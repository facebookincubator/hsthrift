#include "cpp/HsStruct.h"

#include "cpp/Destructible.h"

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

HS_DEFINE_DESTRUCTIBLE(HsMaybeInt, HsMaybe<int64_t>);
HS_DEFINE_DESTRUCTIBLE(HsMaybeDouble, HsMaybe<double>);
HS_DEFINE_DESTRUCTIBLE(HsMaybeString, HsMaybe<HsString>);

HS_DEFINE_DESTRUCTIBLE(HsEitherStringInt, HsEither<HsString, int64_t>);
HS_DEFINE_DESTRUCTIBLE(HsEitherStringDouble, HsEither<HsString, double>);
HS_DEFINE_DESTRUCTIBLE(HsEitherStringString, HsEither<HsString, HsString>);

HS_DEFINE_DESTRUCTIBLE(
    HsEitherStringArrayInt,
    HsEither<HsString, HsArray<int64_t>>);
HS_DEFINE_DESTRUCTIBLE(
    HsEitherStringArrayDouble,
    HsEither<HsString, HsArray<double>>);
HS_DEFINE_DESTRUCTIBLE(
    HsEitherStringArrayString,
    HsEither<HsString, HsArray<HsString>>);

HS_DEFINE_DESTRUCTIBLE(HsString, HsString);

HS_DEFINE_DESTRUCTIBLE(HsArrayInt, HsArray<int64_t>);
HS_DEFINE_DESTRUCTIBLE(HsArrayDouble, HsArray<double>);
HS_DEFINE_DESTRUCTIBLE(HsArrayString, HsArray<HsString>);

HS_DEFINE_DESTRUCTIBLE(HsMapIntInt, HsMap<int64_t, int64_t>);
HS_DEFINE_DESTRUCTIBLE(HsMapIntDouble, HsMap<int64_t, double>);
HS_DEFINE_DESTRUCTIBLE(HsMapIntString, HsMap<int64_t, HsString>);
HS_DEFINE_DESTRUCTIBLE(HsMapStringInt, HsMap<HsString, int64_t>);
HS_DEFINE_DESTRUCTIBLE(HsMapStringDouble, HsMap<HsString, double>);
HS_DEFINE_DESTRUCTIBLE(HsMapStringString, HsMap<HsString, HsString>);

HS_DEFINE_DESTRUCTIBLE(HsJSON, HsJSON);
