// Copyright (c) Facebook, Inc. and its affiliates.

struct B {
  1: i16 a = 1,
  2: i32 b,
  3: i64 c,
}

struct C {
  1: list<Number> x,
  2: list<Number_Strict> y,
  3: B z
}

enum Number {
  Zero = 0,
  One = 1,
  Two = 2,
  Three = 3,
}

enum Number_Strict {
  Zero = 0,
}(hs.nounknown)

enum Number_Discontinuous {
  Five = 5,
  Zero = 0,
}

enum Number_Empty {
}

typedef i64 Int (hs.newtype)

# All the Base types as constants
const byte byte_value = 0
const i16 i16_value = 1
const i32 i32_value = 2
const i64 i64_value = 3
const float float_value = 0.5
const double double_value = 3.14159
const bool bool_value = true
const string string_value = "xxx"
const binary binary_value = "yyy"
const Int newtype_value = 10

# Some Collection Types
const list<i64> list_value = [0, i64_value]
const set<string> set_value = [string_value, ""]
# set<float> is undefined - can't handle NaN.
# const set<double> (cpp.template = 'std::unordered_set') hash_set_value = [0.1, 0.2]
const map<i64, bool> map_value = { 0: true, 1: false }
const map<string, string> (hs.type = "HashMap")
  hash_map_value = { "a" : "A", "b" : "B" }

const B struct_value = { "a": 1, "b": 2, "c": 3 }
const B explicit_struct_value = B { a = 1, b = 2, c = 3 }
const C explicit_nested_struct_value = C {
  x = [],
  y = [],
  z = B { a = 1, b = 2, c = 3 }
}
