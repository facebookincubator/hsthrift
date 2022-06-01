// Copyright (c) Facebook, Inc. and its affiliates.

#include "tests/HsStructHelper.h"

#include <map>
#include <string>
#include "cpp/HsStruct.h"

using namespace std;
using namespace std::string_literals;

namespace facebook::common::hs {

HS_DEFINE_DESTRUCTIBLE(HsMaybeNonmovable, HsMaybe<Nonmovable>);

extern "C" {

HsMaybe<Nonmovable>* getHsMaybeNonmovable() noexcept {
  // test the std::unique_ptr constrcutor
  static HsMaybe<Nonmovable> ret(std::make_unique<Nonmovable>(9, "Crino"s));
  return &ret;
}

HsMaybe<Nonmovable>* createHsMaybeNonmovable(
    int64_t resource,
    const char* str,
    int64_t len) noexcept {
  // test the std::in_place_t constrcutor
  HsMaybe<Nonmovable> ret(std::in_place, resource, std::string(str, len));
  // HsMaybe itself is still safely movable
  return new HsMaybe<Nonmovable>(std::move(ret));
}

} // extern "C"

} // namespace facebook::common::hs

HS_STD_VARIANT_CPP(MyCppVariant);
HS_OPTION_CPP(MyCppVariant, hs_std_variant::MyCppVariant);

extern "C" {

bool checkHsText(HsString val, const char* str, size_t len) {
  return val.getStr() == std::string(str, len);
}

bool checkHsEither(HsEither<HsString, int>* val, int left_or_right) {
  switch (left_or_right) {
    case 1:
      return val->getLeft().getStr() == "string from haskell";
    case 2:
      return val->getRight() == 42;
    default:
      throw std::runtime_error(std::string(
          "checkHsEither: left_or_right should be either 1 or 2. Given: %d",
          left_or_right));
  }
}

HsMaybe<HsString>* getNothing() noexcept {
  static HsMaybe<HsString> ret;
  return &ret;
}

HsMaybe<HsString>* getJust() noexcept {
  static HsMaybe<HsString> ret("just"s);
  return &ret;
}

HsEither<HsString, int64_t>* getLeft() noexcept {
  static HsEither<HsString, int64_t> ret(HsLeft, "error"s);
  return &ret;
}

HsEither<HsString, int64_t>* getRight() noexcept {
  static HsEither<HsString, int64_t> ret(HsRight, 42);
  return &ret;
}

HsArray<HsString>* getArray() noexcept {
  static HsArray<HsString> ret{"foo"s, "bar"s};
  return &ret;
}

HsArray<int64_t>* getArrayInt64() noexcept {
  static HsArray<int64_t> ret{1, 2, 3};
  return &ret;
}

// HsArrayBool uses bytes because std::vector<bool> does
// not use a contiguous array representation
HsArray<uint8_t>* getArrayCBool() noexcept {
  static HsArray<uint8_t> ret{true, false, true, true, true, false};
  return &ret;
}

HsIntMap<int64_t>* getIntMap() noexcept {
  static HsIntMap<int64_t> ret{{2, 4}, {3, 9}, {5, 25}, {7, 49}};
  return &ret;
}

HsPair<HsString, int64_t>* getPair() noexcept {
  static auto ret = HsPair<HsString, int64_t>("foo"s, 3);
  return &ret;
}

using HsNested = HsObject<HsIntMap<HsArray<HsMaybe<HsString>>>>;

HsNested* createNested() noexcept {
  map<string, map<int64_t, vector<folly::Optional<HsString>>>> ret;
  ret["zero"];
  ret["one"][1];
  ret["two"][2].emplace_back(folly::none);
  auto& more = ret["more"];
  more[3].emplace_back(folly::none);
  more[4].emplace_back("two"s);
  more[5].emplace_back(folly::none);
  more[5].emplace_back(""s);
  more[6].emplace_back("two"s);
  more[6].emplace_back("three"s);
  return new HsNested(std::move(ret));
}

void destroyNested(HsNested* p) noexcept {
  delete p;
}
}
