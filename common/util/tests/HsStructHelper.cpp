// Copyright (c) Facebook, Inc. and its affiliates.

#include <map>
#include <string>
#include "cpp/HsStruct.h"

using namespace std;
using namespace std::string_literals;

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
