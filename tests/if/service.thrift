// Copyright (c) Facebook, Inc. and its affiliates.

struct Z {
  1: string name
}

const Z z = { "name": "Z" }

service MyService {
  i64 testFunc(1: i64 arg1, 2: Z arg2 = z)

  void foo() throws (1: Ex ex) (hs.prefix = "x_")
}

service X {
  i32 testFunc() (priority = 'HIGH')
}

service Y extends X {}

struct TestFunc {}

exception Ex {}
