/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <glog/logging.h>
#include <cstddef>
#include <string_view>

#include <folly/Memory.h>
#include <folly/Optional.h>
#include <folly/Try.h>
#include <folly/json/dynamic.h>
#include <optional>

#include "cpp/HsOption.h"
#include "cpp/HsStdTuple.h"
#include "cpp/HsStdVariant.h"
#include "cpp/HsStructDefines.h"
#include "cpp/Marshallable.h"

#define HS_PEEKABLE(t)              \
  static_assert(                    \
      std::is_standard_layout_v<t>, \
      #t " is a standard-layout class so we can peek it")
#define HS_POKEABLE(t)                     \
  static_assert(                           \
      std::is_trivially_destructible_v<t>, \
      #t " is trivially destructible so we can poke it")

template <typename T>
HS_STRUCT HsRange {
  const T* a = nullptr;
  size_t n = 0;

 public:
  HsRange() {}

  /* implicit */ HsRange(folly::Range<const T*> s) : a(s.data()), n(s.size()) {}

  HsRange(const T* a, size_t n) : a(a), n(n) {}

  template <
      typename U = T,
      typename = typename std::enable_if<std::is_same<U, char>::value>::type>
  explicit HsRange(std::string_view sv) : a(sv.data()), n(sv.size()) {}

  const T* data() const {
    return a;
  }

  size_t size() const {
    return n;
  }

  /* implicit */ operator folly::Range<const T*>() const {
    return folly::Range<const T*>(a, n);
  }

  template <
      typename U = T,
      typename = typename std::enable_if<std::is_same<U, char>::value>::type>
  std::string_view toStdStringView() {
    return std::string_view(a, n);
  }
};

using HsStringPiece = HsRange<char>;

using DummyHsRange = HsRange<std::nullptr_t>;
HS_PEEKABLE(DummyHsRange);
HS_POKEABLE(DummyHsRange);
static_assert(
    sizeof(HsStringPiece) == sizeof(DummyHsRange),
    "HsStringPiece is of the same size as DummyHsRange");

template <typename T>
HS_STRUCT HsMaybe {
  const T* ptr = nullptr;

 public:
  HsMaybe() {}

  /* implicit */ HsMaybe(T && value) : ptr(new T(std::move(value))) {}

  /* implicit */ HsMaybe(std::unique_ptr<T> && value) : ptr(value.release()) {}

  template <typename U>
  /* implicit */ HsMaybe(folly::Optional<U> && value) {
    if (value.hasValue()) {
      ptr = new T(std::move(value).value());
    }
  }

  template <typename U>
  /* implicit */ HsMaybe(std::optional<U> && value) {
    if (value.has_value()) {
      ptr = new T(std::move(value).value());
    }
  }

  template <typename... Args>
  explicit HsMaybe(std::in_place_t, Args && ... args)
      : ptr(new T(std::forward<Args>(args)...)) {}

  HsMaybe(const HsMaybe&) = delete;

  HsMaybe(HsMaybe && other) noexcept : ptr(other.ptr) {
    other.ptr = nullptr;
  }

  HsMaybe& operator=(const HsMaybe&) = delete;

  HsMaybe& operator=(HsMaybe&& other) noexcept {
    if (this != &other) {
      delete ptr;
      ptr = other.ptr;
      other.ptr = nullptr;
    }
    return *this;
  }

  ~HsMaybe() {
    delete ptr;
  }

  bool hasValue() const {
    return ptr != nullptr;
  }

  const T& value() const {
    if (hasValue()) {
      return *ptr;
    } else {
      throw std::logic_error("HsMaybe does not have value");
    }
  }
};

using DummyHsMaybe = HsMaybe<std::nullptr_t>;
HS_PEEKABLE(DummyHsMaybe);
static_assert(
    sizeof(HsMaybe<HsStringPiece>) == sizeof(DummyHsMaybe),
    "HsMaybe<HsStringPiece> is of the same size as DummyHsMaybe");

/*
 * Generic HsEither implementation.
 */

enum HsEitherTag { HS_LEFT, HS_RIGHT };

enum HsLeftTag { HsLeft };

enum HsRightTag { HsRight };

template <typename L, typename R>
HS_STRUCT HsEither {
  bool isLeft;
  union {
    L* left;
    R* right;
  };
  friend void* newHsEither(HsEitherTag, void*);

 public:
  template <typename... Args>
  explicit HsEither(HsLeftTag, Args && ... args)
      : isLeft(true), left(new L(std::forward<Args>(args)...)) {}

  template <typename... Args>
  explicit HsEither(HsRightTag, Args && ... args)
      : isLeft(false), right(new R(std::forward<Args>(args)...)) {}

  template <typename... Args>
  HsEither() {
    static_assert(std::is_default_constructible_v<L>);
    isLeft = true;
    left = new L;
  }

  template <typename T>
  /* implicit */ HsEither(folly::Try<T> && value)
      : isLeft(value.hasException()) {
    if (isLeft) {
      left = new L(std::move(value).exception());
    } else {
      right = new R(std::move(value).value());
    }
  }

  HsEither(const HsEither&) = delete;

  HsEither(HsEither && other) noexcept {
    construct(std::move(other));
  }

  HsEither& operator=(const HsEither&) = delete;

  HsEither& operator=(HsEither&& other) noexcept {
    if (this != &other) {
      destruct();
      construct(std::move(other));
    }
    return *this;
  }

  ~HsEither() {
    destruct();
  }

 private:
  void construct(HsEither<L, R> && other) noexcept {
    DCHECK(this != &other);
    isLeft = other.isLeft;
    if (isLeft) {
      left = new L(std::move(*other.left));
    } else {
      right = new R(std::move(*other.right));
    }
  }

  void destruct() {
    if (isLeft) {
      delete left;
    } else {
      delete right;
    }
  }

  explicit HsEither(HsEitherTag tag, void* val) {
    switch (tag) {
      case HS_LEFT:
        isLeft = true;
        left = static_cast<L*>(val);
        break;
      case HS_RIGHT:
        isLeft = false;
        left = static_cast<R*>(val);
        break;
      default:
        __builtin_unreachable();
    }
  }

 public:
  bool hasLeft() const {
    return isLeft;
  }

  bool hasRight() const {
    return !isLeft;
  }

  const L& getLeft() const {
    if (hasLeft()) {
      return *left;
    } else {
      throw std::logic_error("HsEither does not have left");
    }
  }

  const R& getRight() const {
    if (hasRight()) {
      return *right;
    } else {
      throw std::logic_error("HsEither does not have right");
    }
  }
};

using DummyHsEither = HsEither<std::nullptr_t, std::nullptr_t>;
HS_PEEKABLE(DummyHsEither);
static_assert(
    sizeof(HsEither<HsStringPiece, int64_t>) == sizeof(DummyHsEither),
    "HsEither<HsStringPiece, int64_t> is of the same size as DummyHsEither");

extern void* newHsEither(HsEitherTag, void*);

/*
 * A generic HsPair implementation, which stores pointers to the first and
 * second elements of the pair. This indirection causes poor performance, so
 * if you're using it for e.g. complex pairs "((a,b),(c,d))" or in hot code
 * path - you probably want to spin your own pair implementation with
 * concrete types, like:
 *
 * HS_STRUCT HsPairXY {
 *   X x;
 *   Y y;
 * }
 *
 * For simple cases, this should be fine.
 */
template <typename F, typename S>
HS_STRUCT HsPair {
  F* fst_;
  S* snd_;

 public:
  template <typename U, typename V>
  /* implicit */ HsPair(std::pair<U, V> && pair)
      : fst_(new F(std::move(pair.first))),
        snd_(new S(std::move(pair.second))) {}

  /* implicit */ HsPair(F && f, S && s)
      : fst_(new F(std::move(f))), snd_(new S(std::move(s))) {}

  HsPair(const HsPair&) = delete;

  HsPair(HsPair && other) noexcept
      : fst_(new F(std::move(*other.fst_))),
        snd_(new S(std::move(*other.snd_))) {}

  HsPair& operator=(const HsPair&) = delete;

  HsPair& operator=(HsPair&& other) noexcept {
    *fst_ = std::move(*other.fst_);
    *snd_ = std::move(*other.snd_);
  }

  ~HsPair() {
    delete fst_;
    delete snd_;
  }

  const F& first() const {
    return *fst_;
  }

  const S& second() const {
    return *snd_;
  }
};

using DummyHsPair = HsPair<std::nullptr_t, std::nullptr_t>;
HS_PEEKABLE(DummyHsPair);
static_assert(
    sizeof(HsPair<HsStringPiece, int64_t>) == sizeof(DummyHsPair),
    "HsPair<HsStringPiece, int64_t> is of the same size as DummyHsPair");

HS_STRUCT HsString {
  std::string s_;
  const char* str = s_.data();
  size_t len = s_.size();

 public:
  HsString() {}

  /* implicit */ HsString(std::string s) : s_(std::move(s)) {}

  explicit HsString(const folly::exception_wrapper& e)
      : s_(e.what().toStdString()) {}

  HsString(const HsString& other) noexcept : s_(other.s_) {}

  HsString(HsString && other) noexcept : s_(std::move(other.s_)) {}

  HsString& operator=(const HsString& other) noexcept {
    if (this != &other) {
      s_ = other.s_;
      update();
    }
    return *this;
  }

  HsString& operator=(HsString&& other) noexcept {
    if (this != &other) {
      s_ = std::move(other.s_);
      update();
    }
    return *this;
  }

  HsString& operator=(std::string&& other) noexcept {
    if (&(this->s_) != &other) {
      s_ = std::move(other);
      update();
    }
    return *this;
  }

 private:
  void update() {
    str = s_.data();
    len = s_.size();
  }

 public:
  const std::string& getStr() const {
    return s_;
  }

  size_t size() const {
    return s_.size();
  }

  const char* data() const {
    return s_.data();
  }

  char* mutable_data() {
    update();
    return const_cast<char*>(s_.data());
  }

  void resize(size_t capacity) {
    s_.resize(capacity);
    update();
  }

  void clear() {
    s_.clear();
    update();
  }

  void append(const char* src, int n) {
    s_.append(src, n);
    update();
  }

  void append(const HsString& src) {
    s_.append(src.s_);
    update();
  }

  std::string toStdString() && {
    std::string res(std::move(s_));
    update();
    return res;
  }
};

inline bool operator<(const HsString& lhs, const HsString& rhs) {
  return lhs.getStr() < rhs.getStr();
}

HS_PEEKABLE(HsString);

template <typename T>
HS_STRUCT HsArray {
  static_assert(!std::is_same_v<T, bool>, "Use HsArray<uint8_t> instead");
  std::vector<T> v_;
  const T* a = v_.data();
  size_t n = v_.size();

 public:
  HsArray() {}

  /* implicit */ HsArray(std::vector<T> && v) : v_(std::move(v)) {}

  template <typename InputIterator>
  HsArray(InputIterator first, InputIterator last) : v_(first, last) {}

  template <typename Container>
  HsArray(Container && c)
      : v_(std::make_move_iterator(c.begin()),
           std::make_move_iterator(c.end())) {}

  HsArray(std::initializer_list<T> init) : v_(init) {}

  HsArray(const HsArray& other) : v_(other.v_) {}

  HsArray(HsArray && other) noexcept : v_(std::move(other.v_)) {}

  HsArray& operator=(const HsArray& other) {
    if (this != other) {
      v_ = other.v_;
      update();
    }
    return *this;
  }

  HsArray& operator=(HsArray&& other) noexcept {
    if (this != &other) {
      v_ = std::move(other.v_);
      update();
    }
    return *this;
  }

 private:
  void update() {
    a = v_.data();
    n = v_.size();
  }

 public:
  using Iterator = typename std::vector<T>::iterator;
  using ConstIterator = typename std::vector<T>::const_iterator;

  void reserve(size_t capacity) {
    v_.reserve(capacity);
    update();
  }

  void clear() {
    v_.clear();
    update();
  }

  Iterator begin() {
    return v_.begin();
  }

  Iterator end() {
    return v_.end();
  }

  ConstIterator begin() const {
    return v_.begin();
  }

  ConstIterator end() const {
    return v_.end();
  }

  template <typename... Args>
  void add(Args && ... args) {
    v_.emplace_back(std::forward<Args>(args)...);
    update();
  }

  T& operator[](size_t index) {
    return v_[index];
  }

  const T& operator[](size_t index) const {
    return v_[index];
  }

  size_t size() const {
    return v_.size();
  }

  std::vector<T> toStdVector() && {
    auto res = std::move(v_);
    clear();
    return res;
  }
};

using DummyHsArray = HsArray<std::nullptr_t>;
HS_PEEKABLE(DummyHsArray);
static_assert(
    sizeof(HsArray<HsString>) == sizeof(DummyHsArray),
    "HsArray<HsString> is of the same size as DummyHsArray");
static_assert(sizeof(HsArray<uint8_t>) == sizeof(DummyHsArray));

#define HS_DEFINE_ARRAY_CONSTRUCTIBLE(Name, Type)                          \
  extern "C" HsArray<Type>* vector_newHsArray##Name(size_t len) {          \
    auto arr = new HsArray<Type>();                                        \
    arr->reserve(len);                                                     \
    return arr;                                                            \
  }                                                                        \
                                                                           \
  extern "C" void vector_constructHsArray##Name(                           \
      HsArray<Type>* arr, size_t len) {                                    \
    new (arr) HsArray<Type>();                                             \
    arr->reserve(len);                                                     \
  }                                                                        \
                                                                           \
  extern "C" void vector_addHsArray##Name(HsArray<Type>* arr, Type* val) { \
    arr->add(std::move(*val));                                             \
  }

template <typename Key>
HS_STRUCT HsSet {
  std::vector<Key> k_;
  const Key* keys = k_.data();
  size_t n = k_.size();

 public:
  HsSet() {}

  /* implicit */ HsSet(std::vector<Key> && k) : k_(std::move(k)) {}

  /* implicit */ HsSet(std::unordered_set<Key> && s) {
    std::vector<Key> k;
    k.reserve(s.size());
    while (!s.empty()) {
      auto nh = s.extract(s.begin());
      k.insert(std::move(nh.value()));
    }
    this->k_ = k;
    update();
  }

  template <typename InputIterator>
  /* implicit */ HsSet(InputIterator first, InputIterator last) {
    reserve(std::distance(first, last));
    for (auto it = first; it != last; ++it) {
      auto val = *it;
      add(std::move(val));
    }
    update();
  }

  template <typename Container>
  /* implicit */ HsSet(Container && c)
      : HsSet(
            std::make_move_iterator(c.begin()),
            std::make_move_iterator(c.end())) {}

  /* implicit */ HsSet(std::initializer_list<Key> init)
      : HsSet(init.begin(), init.end()) {}

  HsSet(const HsSet&) = delete;

  HsSet(HsSet && other) noexcept : k_(std::move(other.k_)) {
    update();
  }

  HsSet& operator=(const HsSet&) = delete;

  HsSet& operator=(HsSet&& other) noexcept {
    if (this != &other) {
      k_ = std::move(other.k_);
      update();
    }

    return *this;
  }

 private:
  void update() {
    keys = k_.data();
    n = k_.size();
  }

 public:
  struct ConstIterator {
    const HsSet& object;
    size_t index = 0;

    explicit ConstIterator(const HsSet& object) : object(object) {}

    bool isValid() const {
      return index < object.n;
    }

    void next() {
      ++index;
    }

    const Key& get() const {
      DCHECK(isValid());
      return object.keys[index];
    }

    ConstIterator getIterator() const {
      return ConstIterator(*this);
    }
  };

  void reserve(size_t capacity) {
    k_.reserve(capacity);
    update();
  }

  void clear() {
    k_.clear();
    update();
  }

  std::vector<Key> toStdVector() && {
    auto res = std::move(k_);
    update();
    return res;
  }

  std::unordered_set<Key> toStdUnorderedSet() && {
    auto k = std::move(k_);
    std::unordered_set<Key> res(
        std::make_move_iterator(k.begin()), std::make_move_iterator(k.end()));
    update();
    return res;
  }

  template <typename Arg>
  void add(Arg && arg) {
    k_.emplace_back(std::forward<Arg>(arg));
    update();
  }
};

using DummyHsSet = HsSet<std::nullptr_t>;
HS_PEEKABLE(DummyHsSet);
static_assert(
    sizeof(HsSet<HsString>) == sizeof(DummyHsSet),
    "HsSet<HsString> is of the same size as DummyHsSet");
static_assert(sizeof(HsSet<uint8_t>) == sizeof(DummyHsSet));

#define HS_DEFINE_SET_CONSTRUCTIBLE(Name, Type)                            \
  extern "C" void set_constructHsSet##Name(HsSet<Type>* set, size_t len) { \
    new (set) HsSet<Type>();                                               \
    set->reserve(len);                                                     \
  }                                                                        \
                                                                           \
  extern "C" void set_addHsSet##Name(HsSet<Type>* set, Type* val) {        \
    set->add(std::move(*val));                                             \
  }

template <typename Key, typename Value>
HS_STRUCT HsMap {
  std::vector<Key> k_;
  std::vector<Value> v_;
  const Key* keys = k_.data();
  const Value* values = v_.data();
  size_t n = k_.size();

 public:
  HsMap() {}

  HsMap(std::vector<Key> && k, std::vector<Value> && v)
      : k_(std::move(k)), v_(std::move(v)) {
    DCHECK(k_.size() == v_.size());
  }

  template <typename InputIterator>
  HsMap(InputIterator first, InputIterator last) {
    reserve(std::distance(first, last));
    for (auto it = first; it != last; ++it) {
      // when `it` is a `move_iterator`, the content will be moved;
      // otherwise, the content will be copied.
      auto value = *it;
      add(std::move(value.first), std::move(value.second));
    }
  }

  template <typename Container>
  HsMap(Container && c)
      : HsMap(
            std::make_move_iterator(c.begin()),
            std::make_move_iterator(c.end())) {}

  HsMap(std::initializer_list<std::pair<const Key, Value>> init)
      : HsMap(init.begin(), init.end()) {}

  HsMap(const HsMap&) = delete;

  HsMap(HsMap && other) noexcept
      : k_(std::move(other.k_)), v_(std::move(other.v_)) {}

  HsMap& operator=(const HsMap&) = delete;

  HsMap& operator=(HsMap&& other) noexcept {
    if (this != &other) {
      k_ = std::move(other.k_);
      v_ = std::move(other.v_);
      update();
    }
    return *this;
  }

 private:
  void update() {
    DCHECK(k_.size() == v_.size());
    keys = k_.data();
    values = v_.data();
    n = k_.size();
  }

 public:
  struct ConstIterator {
    const HsMap& object;
    size_t index = 0;

    explicit ConstIterator(const HsMap& object) : object(object) {}

    bool isValid() const {
      return index < object.n;
    }

    void next() {
      ++index;
    }

    const Key& getKey() const {
      DCHECK(isValid());
      return object.keys[index];
    }

    const Value& getValue() const {
      DCHECK(isValid());
      return object.values[index];
    }
  };

  ConstIterator getIterator() const {
    return ConstIterator(*this);
  }

  void reserve(size_t capacity) {
    k_.reserve(capacity);
    v_.reserve(capacity);
    update();
  }

  void clear() {
    k_.clear();
    v_.clear();
    update();
  }

  std::pair<std::vector<Key>, std::vector<Value>> take_items() && {
    auto res = std::make_pair(std::move(k_), std::move(v_));
    update();
    return res;
  }

  template <typename Arg, typename... Args>
  void add(Arg && arg, Args && ... args) {
    k_.emplace_back(std::forward<Arg>(arg));
    try {
      v_.emplace_back(std::forward<Args>(args)...);
    } catch (...) {
      k_.pop_back();
      throw;
    }
    update();
  }

  // This is a linear lookup which is supposed to be used when you only want
  // to marshal a single entry of the whole map (e.g. in fbobjAttr).
  const Value* getPtr(const Key& key) const {
    // The last mapping overrides the previous ones
    for (int i = static_cast<int>(k_.size()) - 1; i >= 0; --i) {
      if (k_[i] == key) {
        return &v_[i];
      }
    }
    return nullptr;
  }

  template <typename T>
  typename std::enable_if<
      std::is_same<Key, HsString>::value &&
          std::is_convertible<T, folly::StringPiece>::value,
      const Value*>::type
  getPtr(const T& key) const {
    // The last mapping overrides the previous ones
    for (int i = static_cast<int>(k_.size()) - 1; i >= 0; --i) {
      if (folly::StringPiece(k_[i].getStr()) == key) {
        return &v_[i];
      }
    }
    return nullptr;
  }
};

#define HS_DEFINE_MAP_CONSTRUCTIBLE(Name, KeyType, ValType)       \
  extern "C" void map_constructHsMap##Name(                       \
      HsMap<KeyType, ValType>* map, size_t len) {                 \
    new (map) HsMap<KeyType, ValType>();                          \
    map->reserve(len);                                            \
  }                                                               \
  extern "C" void map_addHsMap##Name(                             \
      HsMap<KeyType, ValType>* map, KeyType* key, ValType* val) { \
    map->add(std::move(*key), std::move(*val));                   \
  }

template <typename T>
using HsIntMap = HsMap<int64_t, T>;
using DummyHsIntMap = HsIntMap<std::nullptr_t>;
HS_PEEKABLE(DummyHsIntMap);

template <typename T>
using HsObject = HsMap<HsString, T>;
using DummyHsObject = HsObject<std::nullptr_t>;
HS_PEEKABLE(DummyHsObject);

static_assert(
    sizeof(HsIntMap<HsString>) == sizeof(DummyHsIntMap) &&
        sizeof(HsObject<HsString>) == sizeof(DummyHsObject) &&
        sizeof(DummyHsIntMap) == sizeof(DummyHsObject),
    "All HsMap<Key, Value> must have the same size");

class HsJSON {
 public:
  enum class Type {
    Null,
    Bool,
    Integral,
    Real,
    String,
    Array,
    Object,
  };

#ifdef __HSC2HS__
 public:
#else
 private:
#endif

  Type type;
  union {
    std::nullptr_t data;
    int64_t integral;
    double real;
    HsString string;
    HsArray<HsJSON> array;
    HsObject<HsJSON> object;
  };

 public:
  /* implicit */ HsJSON() : type(Type::Null) {}

  /* implicit */ HsJSON(bool value) : type(Type::Bool), integral(value) {}

  /* implicit */ HsJSON(int64_t value)
      : type(Type::Integral), integral(value) {}

  /* implicit */ HsJSON(double value) : type(Type::Real), real(value) {}

  /* implicit */ HsJSON(HsString&& value)
      : type(Type::String), string(std::move(value)) {}

  /* implicit */ HsJSON(HsArray<HsJSON>&& value)
      : type(Type::Array), array(std::move(value)) {}

  /* implicit */ HsJSON(HsObject<HsJSON>&& value)
      : type(Type::Object), object(std::move(value)) {}

  /* implicit */ HsJSON(const folly::dynamic& value);

  HsJSON(const HsJSON&) = delete;

  HsJSON(HsJSON&& other) noexcept {
    construct(std::move(other));
  }

  HsJSON& operator=(const HsJSON&) = delete;

  HsJSON& operator=(HsJSON&& other) noexcept {
    if (this != &other) {
      destruct();
      construct(std::move(other));
    }
    return *this;
  }

  ~HsJSON() {
    destruct();
  }

 private:
  void construct(HsJSON&& other);

  void destruct();

 public:
  Type getType() const {
    return type;
  }

  int64_t asIntegral() const {
    DCHECK(type == Type::Bool || type == Type::Integral);
    return integral;
  }

  int64_t& asIntegral() {
    DCHECK(type == Type::Bool || type == Type::Integral);
    return integral;
  }

  double asReal() const {
    DCHECK(type == Type::Real);
    return real;
  }

  double& asReal() {
    DCHECK(type == Type::Real);
    return real;
  }

  const HsString& asString() const {
    DCHECK(type == Type::String);
    return string;
  }

  HsString& asString() {
    DCHECK(type == Type::String);
    return string;
  }

  const HsArray<HsJSON>& asArray() const {
    DCHECK(type == Type::Array);
    return array;
  }

  HsArray<HsJSON>& asArray() {
    DCHECK(type == Type::Array);
    return array;
  }

  const HsObject<HsJSON>& asObject() const {
    DCHECK(type == Type::Object);
    return object;
  }

  HsObject<HsJSON>& asObject() {
    DCHECK(type == Type::Object);
    return object;
  }

  folly::dynamic toDynamic() &&;
};

HS_PEEKABLE(HsJSON);

HS_OPTION_H(Bool, bool)
HS_OPTION_H(UInt8, uint8_t)
HS_OPTION_H(Int16, int16_t)
HS_OPTION_H(Int32, int32_t)
HS_OPTION_H(Int64, int64_t)
HS_OPTION_H(UInt32, uint32_t)
HS_OPTION_H(UInt64, uint64_t)
HS_OPTION_H(Float, float)
HS_OPTION_H(Double, double)
HS_OPTION_H(String, HsString)
HS_OPTION_H(StringView, HsStringPiece)
HS_OPTION_H(HsJSON, HsJSON)
