// Copyright (c) Facebook, Inc. and its affiliates.

# @nolint

# This file is gonna be uge

# HIERARCHY OF STUFF
#
#     A       G H I J K L M N O P Q R S T U V W
#    / \                                     / \
#   B   C                                   X   Y
#      / \                                 /
#     D   E                               Z -> W
#      \ / \
#       F  foo
#           |
#          bar

hs_include "if/huge.hs"

struct A {
  1: B b
  2: C c
}

struct B {}

struct C {
  1: D d
  2: E e
}

struct D {
  1: F f
}

struct E {
  1: list<F> f = foo
}

enum F {
  F = 0,
}

const list<F> foo = [ bar ]
const F bar = F

enum G {
  G = 0,
}
enum H {
  H = 0,
}
struct I {}
struct J {}
typedef i64 K
typedef i64 L
enum M {
  M = 0,
}
enum N {
  N = 0,
}
struct O {}
struct P {}
typedef i64 Q
typedef i64 R
enum S {
  S = 0,
}
enum T {
  T = 0,
}
struct U {}
struct V {}

# This stuff is mututally recursive
typedef map<Y, X> W

typedef Z X

enum Y {
  Y = 0,
}

struct Z {
  1: W w
}

service Service {
  void weNeedThis(1: i64 x)
  void weDontNeedThis(1: string x)
}

struct HsInclude {}
struct HsInclude2 {
  1: HsInclude get
}

const HsInclude hsconst = {}
