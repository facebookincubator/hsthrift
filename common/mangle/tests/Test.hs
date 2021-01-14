-- Copyright (c) Facebook, Inc. and its affiliates.

module Test where

import Test.HUnit
import TestRunner

import Mangle

main :: IO ()
main = testRunner tests

tests :: Test
tests = TestList $ map (uncurry3 assertMangle)
  [ eg "simple"
    "_Z7examplei"
    "int example(int)"

  , eg "void"
    "_Z7examplev"
    "int example(void)"

  , eg "implicitly void"
    "_Z7examplev"
    "int example()"

  , eg "simple cv"
    "_Z7examplePKc"
    "int example(const char*)"

  , eg "qualifiers"
    "_Z7examplej"
    "void example(unsigned int)"

  , eg "qualifier ordering"
    "_Z7examplem"
    "void example(long unsigned int)"

  , eg "qualifier without type"
    "_Z7examplej"
    "void example(unsigned)"

  , eg "qualifiers without type"
    "_Z7examplem"
    "void example(unsigned long)"

  , eg "long as singular argument"
    "_Z7examplel"
    "void example(long)"

  , eg "doubled qualifiers"
    "_Z7exampley"
    "void example(unsigned long long int)"

  , eg "multiple cv"
    "_Z7examplePVKi"
    "int example(const int volatile*)"

  , eg "multiple cv order"
    "_Z7examplePVKi"
    "int example(volatile int const*)"

  , eg "qualified name"
    "_ZN5Class8functionEi"
    "int Class::function(int)"

  , eg "multiple qualified names"
    "_ZN2ns5Class8functionERKN5other5thingEi"
    "int ns::Class::function(const other::thing&, int)"

  , eg "std::string"
    "_Z7exampleRKSs"
    "void example(const std::string&)"

  , eg "std::shared_ptr"
    "_Z7exampleSt10shared_ptrIiE"
    "void example(std::shared_ptr<int>)"

  , eg "boost::shared_ptr"
    "_Z7exampleN5boost10shared_ptrIiEE"
    "void example(boost::shared_ptr<int>)"

  , eg "unqualified templated names"
    "_Z7example7MyClassIiiE"
    "void example(MyClass<int,int>)"

  , eg "double pointer"
    "_Z7examplePPc"
    "void example(char**)"

  , eg "double pointer with cv"
    "_Z7examplePKPKc"
    "void example(const char* const*)"

  , eg "reference"
    "_Z7exampleRm"
    "void example(unsigned long&)"

  , eg "member function cv"
    "_ZNK3Foo10frobnicateEv"
    "void Foo::frobnicate() const"

  , eg "simple substitution"
    "_Z7example3fooS_"
    "void example(foo, foo)"

  , eg "simple builtin substitution"
    "_Z7examplePKcS0_"
    "void example(const char*, const char*)"

  , eg "no substitution of base types"
    "_Z7examplemm"
    "void example(unsigned long, unsigned long)"

  , eg "internal substitution"
    "_Z7examplePKcPKS0_"
    "void example(const char*, const char* const*)"

  , eg "internal substitution with skipping"
    "_Z7examplemPPmS_"
    "void example(uint64_t, uint64_t**, uint64_t*)"

  , eg "multiple substitutions"
    "_Z7examplePKcS0_PKS0_"
    "void example(const char*, const char*, const char* const*)"

  , eg "multiple substitutions in nested name (not monotonic)"
    "_ZNK6nested7exampleEmPKPKmS1_"
    "void nested::example(uint64_t, uint64_t const* const*, uint64_t const*) const"

  , eg "substitution in nested name"
    "_ZN6nested7exampleEPKcS1_"
    "void nested::example(const char*, const char*)"

  , eg "substitution of prefix"
    "_ZN6nested7exampleENS_5thingE"
    "void nested::example(nested::thing)"

  , eg "substitution of whole nested name"
    "_Z7exampleN6nested5thingES0_"
    "void example(nested::thing, nested::thing)"

  , eg "substitution of nested namespaces"
    "_ZN2fb2hs7exampleENS_1AEPKS1_NS0_1BES3_"
    "void fb::hs::example(fb::A, const fb::A*, fb::hs::B, fb::A const*)"

  -- TODO(watashi): t9761416 make following tests pass
  -- , eg "substitution of std"
  --   "_ZSt7exampleNSt3std1AES0_NS_3stdE"
  --   "void std::example(std::A, std::std::A, std::std::std)"

  , eg "substitution of types with qualifiers"
     "_Z7example1AS_PS_PKS_S2_S0_"
     "void example(A, const A, A*, const A*, A const*, A* const)"

  -- , eg "substitution of partial namespaces"
  --   "_Z7exampleN6outter1AENS_5inner1AENS_1BENS1_1BE"
  --   "void example(outter::A, outter::inner::A, outter::B, outter::inner::B)"

  , eg "std::size_t"
    "_Z13c_get_counterPN1A1B1CEPKcm"
    "int64_t c_get_counter(A::B::C*, const char*, std::size_t)"

  , eg "compression"
    "_ZN8facebook2hs16readDynamicArrayEPKN5folly7dynamicEmPS4_"
    "int facebook::hs::readDynamicArray(const folly::dynamic*, size_t, const folly::dynamic**)"
  ]
  where
    eg = (,,)
    uncurry3 f (a, b, c) = f a b c

assertRight :: (Show a, Show b, Eq b) => String -> b -> Either a b -> Assertion
assertRight prefix expected actual
  = assertBool message $ rightly expected actual
  where
  message = concat
    [ prefix
    , ": expected "
    , show expected
    , " but got "
    , show actual
    ]

rightly :: (Eq b) => b -> Either a b -> Bool
rightly = either (const False) . (==)

assertMangle :: String -> String -> String -> Test
assertMangle prefix expected
  = TestLabel prefix . TestCase . assertRight prefix expected . mangle
