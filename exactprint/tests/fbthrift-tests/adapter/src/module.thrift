// THIS FILE IS COPIED FROM FBTHRIFT, DO NOT MODIFY ITS CONTENTS DIRECTLY
// generated-by : fbcode/common/hs/thrift/exactprint/tests/sync-fbthrift-tests.sh
// source: thrift/compiler/test/fixtures/*
// @generated
/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

namespace android test.fixtures.adapter
namespace java test.fixtures.adapter
namespace java.swift test.fixtures.adapter

include "thrift/annotation/cpp.thrift"
include "thrift/annotation/python.thrift"
include "thrift/annotation/thrift.thrift"
include "thrift/annotation/hack.thrift"

@thrift.Experimental
package "facebook.com/thrift/test"

@hack.Adapter{name = '\Adapter2'}
@cpp.Adapter{name = 'my::Adapter2'}
typedef set<string> (py.adapter = 'my.Adapter2') SetWithAdapter
@hack.Adapter{name = '\Adapter1'}
@cpp.Adapter{name = 'my::Adapter1'}
typedef string (py.adapter = 'my.Adapter1') StringWithAdapter
typedef list<StringWithAdapter> ListWithElemAdapter
@hack.Adapter{name = '\Adapter2'}
@cpp.Adapter{name = 'my::Adapter2'}
typedef ListWithElemAdapter ListWithElemAdapter_withAdapter

@cpp.Adapter{name = "my::Adapter1"}
@python.Adapter{
  name = "my.module.Adapter2",
  typeHint = "my.another.module.AdaptedType2",
}
typedef i64 MyI64

typedef MyI64 DoubleTypedefI64

@hack.Adapter{name = '\Adapter1'}
@cpp.Adapter{name = 'my::Adapter1'}
typedef i32 MyI32

struct Foo {
  @hack.Adapter{name = '\Adapter1'}
  @cpp.Adapter{name = 'my::Adapter1'}
  1: i32 (py.adapter = 'my.Adapter1') intField;
  @hack.Adapter{name = '\Adapter1'}
  @cpp.Adapter{name = 'my::Adapter1'}
  2: optional i32 (py.adapter = 'my.Adapter1') optionalIntField;
  @hack.Adapter{name = '\Adapter1'}
  @cpp.Adapter{name = 'my::Adapter1'}
  3: i32 (py.adapter = 'my.Adapter1') intFieldWithDefault = 13;
  4: SetWithAdapter setField;
  5: optional SetWithAdapter optionalSetField;
  @hack.Adapter{name = '\Adapter3'}
  @cpp.Adapter{name = 'my::Adapter3'}
  6: map<string, ListWithElemAdapter_withAdapter_2769> (
    py.adapter = 'my.Adapter3',
  ) mapField;
  @hack.Adapter{name = '\Adapter3'}
  @cpp.Adapter{name = 'my::Adapter3'}
  7: optional map<string, ListWithElemAdapter_withAdapter_2769> (
    py.adapter = 'my.Adapter3',
  ) optionalMapField;
  @hack.Adapter{name = '\Adapter1'}
  @cpp.Adapter{name = 'my::Adapter3'}
  8: binary (py.adapter = 'my.Adapter1') binaryField;
  9: MyI64 longField;
  @cpp.Adapter{name = "my::Adapter2"}
  @python.Adapter{name = "my.Adapter3", typeHint = "my.AdaptedType3"}
  10: MyI64 adaptedLongField;
  11: DoubleTypedefI64 doubleAdaptedField;
} (
  thrift.uri = "facebook.com/thrift/compiler/test/fixtures/adapter/src/module/Foo",
)

union Baz {
  @hack.Adapter{name = '\Adapter1'}
  @cpp.Adapter{name = 'my::Adapter1'}
  1: i32 (py.adapter = 'my.Adapter1') intField;
  4: SetWithAdapter setField;
  @hack.Adapter{name = '\Adapter3'}
  @cpp.Adapter{name = 'my::Adapter3'}
  6: map<string, ListWithElemAdapter_withAdapter_2769> (
    py.adapter = 'my.Adapter3',
  ) mapField;
  @hack.Adapter{name = '\Adapter1'}
  @cpp.Adapter{name = 'my::Adapter1'}
  8: binary (py.adapter = 'my.Adapter1') binaryField;
  9: MyI64 longField;
}

@hack.Adapter{name = '\Adapter1'}
@cpp.Adapter{name = 'my::Adapter1'}
typedef Foo FooWithAdapter

struct Bar {
  @hack.Adapter{name = '\Adapter1'}
  @cpp.Adapter{name = 'my::Adapter1', adaptedType = 'my::Cpp::Type1'}
  1: Foo (py.adapter = 'my.Adapter1') structField;
  @hack.Adapter{name = '\Adapter1'}
  @cpp.Adapter{name = 'my::Adapter1'}
  2: optional Foo (py.adapter = 'my.Adapter1') optionalStructField;
  3: list<FooWithAdapter_8945> structListField;
  4: optional list<FooWithAdapter_8945> optionalStructListField;
  @hack.Adapter{name = '\Adapter1'}
  @cpp.Adapter{name = 'my::Adapter1'}
  5: Baz (py.adapter = 'my.Adapter1') unionField;
  @hack.Adapter{name = '\Adapter1'}
  @cpp.Adapter{name = 'my::Adapter1'}
  6: optional Baz (py.adapter = 'my.Adapter1') optionalUnionField;
  7: DirectlyAdapted adaptedStructField;
}

@cpp.Adapter{name = "my::Adapter"}
@python.Adapter{
  name = "my.module.Adapter",
  typeHint = "my.another.module.AdaptedType",
}
struct DirectlyAdapted {
  1: i32 field;
}

struct StructWithFieldAdapter {
  @cpp.Adapter{name = "my::Adapter1"}
  @python.Adapter{name = "my.Adapter1", typeHint = "my.AdaptedType1"}
  1: i32 field;
  @cpp.Adapter{name = "my::Adapter1"}
  @cpp.Ref{type = cpp.RefType.Shared}
  2: i32 shared_field;
  @cpp.Adapter{name = "my::Adapter1"}
  @cpp.Ref{type = cpp.RefType.Shared}
  3: optional i32 opt_shared_field;
  @cpp.Adapter{name = "my::Adapter1"}
  @thrift.Box
  4: optional i32 opt_boxed_field;
}

struct TerseAdaptedFields {
  @cpp.Adapter{name = "my::Adapter1"}
  @thrift.TerseWrite
  1: i32 int_field;
  @cpp.Adapter{name = "my::Adapter1"}
  @thrift.TerseWrite
  2: string string_field;
  @cpp.Adapter{name = "my::Adapter1"}
  @thrift.TerseWrite
  3: set<i32> set_field;
}

@hack.Adapter{name = '\Adapter2'}
@cpp.Adapter{name = 'my::Adapter2'}
typedef Bar (py.adapter = 'my.Adapter2') StructWithAdapter

@hack.Adapter{name = '\Adapter2'}
@cpp.Adapter{name = 'my::Adapter2'}
typedef Baz (py.adapter = 'my.Adapter2') UnionWithAdapter

struct B {
  1: AdaptedA a;
}
@cpp.Adapter{name = "my::Adapter"}
typedef A AdaptedA
struct A {}

service Service {
  MyI32_553 func(
    1: StringWithAdapter_9663 arg1,
    @cpp.Adapter{name = "my::Adapter2"}
    2: string arg2,
    3: Foo arg3,
  );
}

@cpp.Adapter{name = "MyVarAdapter"}
const i32 var1 = 10;

@cpp.Adapter{name = "MyVarAdapter"}
const string var2 = "20";

struct MyStruct {
  1: i32 field;
}

@cpp.Adapter{name = "MyVarAdapter"}
const MyStruct var3 = MyStruct{field = 30};

// The following were automatically generated and may benefit from renaming.
typedef FooWithAdapter (py.adapter = "my.Adapter1") FooWithAdapter_8945
typedef ListWithElemAdapter_withAdapter (
  py.adapter = "my.Adapter2",
) ListWithElemAdapter_withAdapter_2769
typedef MyI32 (py.adapter = "my.Adapter1") MyI32_553
typedef StringWithAdapter (py.adapter = "my.Adapter2") StringWithAdapter_9663
