// Copyright (c) Facebook, Inc. and its affiliates.

typedef i64 X

struct Foo {
  1: optional i64 foo1
  2: bool foo2
  3: Bar foo3
  4: X foo4
  5: list<i64> (hs.type = "Vector") foo5
  7: i64 foo_foo1
}

struct Bar {
  1: i32 bar1
  2: string bar2
  3: optional i64 foo1
  4: i64 foo2
}

struct Huge {
  1: optional i64 foo1
  2: bool foo2
  3: Bar foo3
  4: X foo4
  5: list<i64> (hs.type = "Vector") foo5
  7: i64 foo_foo1
  10: i64 huge10
  11: i64 huge11
  12: i64 huge12
  13: i64 huge13
  14: i64 huge14
  15: i64 huge15
  16: i64 huge16
  17: i64 huge17
  18: i64 huge18
  19: i64 huge19
  20: i64 huge20
  21: i64 huge21
  22: i64 huge22
  23: i64 huge23
  24: i64 huge24
  25: i64 huge25
  26: i64 huge26
  27: i64 huge27
  28: i64 huge28
  29: i64 huge29
  30: i64 huge30
  31: i64 huge31
  32: i64 huge32
  33: i64 huge33
  34: i64 huge34
  35: i64 huge35
  36: i64 huge36
  37: i64 huge37
  38: i64 huge38
  39: i64 huge39
  40: i64 huge40
  41: i64 huge41
  42: i64 huge42
  43: i64 huge43
  44: i64 huge44
  45: i64 huge45
  46: i64 huge46
  47: i64 huge47
  48: i64 huge48
  49: i64 huge49
  50: i64 huge50
  51: i64 huge51
  52: i64 huge52
  53: i64 huge53
  54: i64 huge54
  55: i64 huge55
  56: i64 huge56
  57: i64 huge57
  58: i64 huge58
  59: i64 huge59
  60: i64 huge60
  61: i64 huge61
  62: i64 huge62
  63: i64 huge63
  64: i64 huge64
  65: i64 huge65
  66: i64 huge66
  67: i64 huge67
  68: i64 huge68
  69: i64 huge69
  70: i64 huge70
  71: i64 huge71
  72: i64 huge72
  73: i64 huge73
  74: i64 huge74
  75: i64 huge75
  76: i64 huge76
  77: i64 huge77
  78: i64 huge78
  79: i64 huge79
}
