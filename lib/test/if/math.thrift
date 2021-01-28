// Copyright (c) Facebook, Inc. and its affiliates.

exception DivideByZero {}

service Adder {

  i64 add(1: i64 x, 2: i64 y)

}

service Calculator extends Adder {

  double divide(1: double dividend, 2: double divisor) throws
  (1: DivideByZero divisionError)

  oneway void put(1: i64 val)

  oneway void putMany(1: list<i64> val) (haxl.batched)

  i64 get()

  void unimplemented()

}
