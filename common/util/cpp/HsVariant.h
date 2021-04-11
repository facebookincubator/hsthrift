// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include "cpp/HsStruct.h"

#include <utility>
#include <vector>

struct HsVariant {
  using VariantType = HsJSON;
  using MapType = HsObject<HsJSON>;
  using MapKeyType = HsString;
  using MapIterator = MapType::ConstIterator;
  using VectorType = HsArray<HsJSON>;
  using VectorIterator = VectorType::ConstIterator;
  using SetType = HsArray<HsJSON>;
  using SetIterator = SetType::ConstIterator;
  using StringType = HsString;
  using Type = facebook::serialize::Type;
  using StructHandle = int;

  /**
   * \defgroup as_type variant accessors
   * @{
   */
  static Type type(const VariantType& obj) {
    switch (obj.getType()) {
      case HsJSON::Type::Null:
        return Type::NULLT;
      case HsJSON::Type::Bool:
        return Type::BOOL;
      case HsJSON::Type::Integral:
        return Type::INT64;
      case HsJSON::Type::Real:
        return Type::DOUBLE;
      case HsJSON::Type::String:
        return Type::STRING;
      case HsJSON::Type::Array:
        return Type::VECTOR;
      case HsJSON::Type::Object:
        return Type::MAP;
      default:
        __builtin_unreachable();
    }
  }

  static bool asBool(const VariantType& obj) {
    return obj.asIntegral() != 0;
  }

  static int64_t asInt64(const VariantType& obj) {
    return obj.asIntegral();
  }

  static double asDouble(const VariantType& obj) {
    return obj.asReal();
  }

  static const StringType& asString(const VariantType& obj) {
    return obj.asString();
  }

  static const VectorType& asVector(const VariantType& obj) {
    return obj.asArray();
  }

  static const MapType& asMap(const VariantType& obj) {
    return obj.asObject();
  }

  static const SetType& asSet(const VariantType& obj) {
    return obj.asArray();
  }
  /** @} */

  /**
   * \defgroup to_variant variant creators
   * @{
   */
  static VariantType createNull() {
    return false;
  }
  static VariantType fromInt64(int64_t val) {
    return val;
  }
  static VariantType fromBool(bool val) {
    return val;
  }
  static VariantType fromDouble(double val) {
    return val;
  }
  static VariantType fromString(StringType&& str) {
    return std::move(str);
  }
  static VariantType fromMap(MapType&& map) {
    return std::move(map);
  }
  static VariantType fromVector(VectorType&& vec) {
    return std::move(vec);
  }
  static VariantType fromSet(SetType&& set) {
    return std::move(set);
  }
  /** @} */

  /**
   * \defgroup map map methods
   * @{
   */
  // builders
  static MapType createMap() {
    return MapType();
  }

  static MapType createMap(MapType&& m) {
    return MapType(std::move(m));
  }

  static MapType reserveMap(size_t n) {
    MapType ret;
    ret.reserve(n);
    return ret;
  }

  static MapType reserveMap(StructHandle, size_t n) {
    return reserveMap(n);
  }

  static MapType getStaticEmptyMap() {
    return createMap();
  }

  static MapKeyType mapKeyFromInt64(int64_t key) {
    return HsString(folly::to<std::string>(key));
  }

  static void mapSet(MapType& map, HsString&& key, VariantType&& value) {
    map.add(std::move(key), std::move(value));
  }

  static void mapSet(MapType& map, const HsString& key, VariantType&& value) {
    map.add(key, std::move(value));
  }

  static void mapSet(MapType& map, int64_t key, VariantType&& value) {
    map.add(mapKeyFromInt64(key), std::move(value));
  }

  template <typename T>
  static void
  mapSet(MapType& map, size_t /*idx*/, T&& key, VariantType&& value) {
    mapSet(map, std::forward<T>(key), std::move(value));
  }

  // accessors
  static Type mapKeyType(const MapKeyType& /*map*/) {
    return Type::STRING;
  }

  static int64_t mapKeyAsInt64(const MapKeyType& /*key*/) {
    throw std::runtime_error("mapKeyType should always be Type::STRING");
  }

  static HsString mapKeyAsString(const MapKeyType& key) {
    return key;
  }

  static MapIterator mapIterator(const MapType& map) {
    return map.getIterator();
  }

  static bool mapNotEnd(const MapType& /*map*/, const MapIterator& it) {
    return it.isValid();
  }

  static void mapNext(MapIterator& it) {
    it.next();
  }

  static const HsString& mapKey(const MapIterator& it) {
    return it.getKey();
  }

  static const HsJSON& mapValue(const MapIterator& it) {
    return it.getValue();
  }

  /** @} */

  /**
   * \defgroup vector vector methods
   * @{
   */
  // builders
  static VectorType createVector() {
    return VectorType();
  }

  static int64_t vectorSize(const VectorType& vec) {
    return vec.size();
  }

  static void vectorAppend(VectorType& vec, VariantType&& v) {
    vec.add(std::move(v));
  }

  // accessors
  static VectorIterator vectorIterator(const VectorType& vec) {
    return vec.begin();
  }

  static bool vectorNotEnd(const VectorType& vec, VectorIterator& it) {
    return it != vec.end();
  }

  static void vectorNext(VectorIterator& it) {
    ++it;
  }

  static const VariantType& vectorValue(VectorIterator& it) {
    return *it;
  }
  /** @} */

  /**
   * \defgroup set set methods
   * @{
   */
  // builders
  static SetType createSet() {
    return SetType();
  }

  static int64_t setSize(const SetType& set) {
    return set.size();
  }

  static void setAppend(SetType& set, VariantType&& v) {
    set.add(std::move(v));
  }

  // accessors
  static SetIterator setIterator(const SetType& set) {
    return set.begin();
  }

  static bool setNotEnd(const SetType& set, SetIterator& it) {
    return it != set.end();
  }

  static void setNext(SetIterator& it) {
    ++it;
  }

  static const VariantType& setValue(SetIterator& it) {
    return *it;
  }
  /** @} */

  /**
   * \defgroup string string methods
   * @{
   */
  static StringType createString() {
    return std::string();
  }

  static StringType createMutableString(size_t n) {
    return std::string(n, '\0');
  }

  static char* getMutablePtr(StringType& str) {
    return str.mutable_data();
  }

  static void shrinkString(StringType& str, size_t n) {
    str.resize(n);
  }

  static StringType stringFromData(const char* src, int n) {
    return std::string(src, n);
  }

  static StringType stringFromMutableString(StringType&& s) {
    return std::move(s);
  }

  static StringType createStaticString(const char* src, int n) {
    return stringFromData(src, n);
  }

  static StringType getStaticEmptyString() {
    return std::string();
  }

  static void stringClear(StringType& str) {
    str.clear();
  }

  static int stringLen(const StringType& str) {
    return str.size();
  }

  static const char* stringData(const StringType& str) {
    return str.data();
  }

  static void stringAppend(StringType& str, const char* src, int n) {
    str.append(src, n);
  }

  static void stringAppend(StringType& str, const StringType& src) {
    str.append(src);
  }

  static void traceSerialization(const VariantType&) {}

  static StructHandle registerStruct(
      const StringType& stableIdentifier,
      const std::vector<std::pair<size_t, StringType>>& fields) {
    return -1;
  }
  /** @} */
};
