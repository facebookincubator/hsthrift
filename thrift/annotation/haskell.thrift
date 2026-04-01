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

include "thrift/annotation/scope.thrift"
include "thrift/annotation/thrift.thrift"

package "facebook.com/thrift/annotation/haskell"

@scope.Typedef
@scope.Field
@scope.FunctionParameter
struct Type {
  1: string name;
}

@scope.Struct
@scope.Field
struct Lazy {}

@scope.Struct
@scope.Field
struct Strict {}

@scope.Field
struct Hidden {}

@scope.Typedef
struct Newtype {}

@scope.Union
struct NonEmpty {}

@scope.Enum
struct NoUnknown {}

@scope.Enum
struct PseudoEnum {
  1: string value;
}

@scope.Struct
@scope.Union
@scope.Enum
@scope.Function
struct Prefix {
  1: string name;
}
