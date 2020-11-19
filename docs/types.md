# Haskell Thrift Types

## Base Types

Thrift has several base types. Each base type corresponds to a default Haskell type. In some cases, the generated Haskell type can be modified using annotations or compiler options. These are the defaults:

| Thrift Type | Haskell Type
| ----------- | ------------
| `binary`    | `ByteString`
| `bool`      | `Bool`
| `byte`      | `Int8`
| `i16`       | `Int16`
| `i32`       | `Int32`
| `i64`       | `Int64`
| `float`     | `Float`
| `double`    | `Double`
| `string`    | `Text`

## Collections

Technically collection types can contain any other Thrift types, however in Haskell some of them have constraints. This table gives the default translations as well as constrains on the inner types.

| Thrift Type | Haskell Type (with constraints)
| ----------- | ---
| `list<a>`   | `[a]`
| `set<a>`    | `(Eq a, Ord a) => Set a`
| `hash_set<a>` | `(Eq a, Hashable a) => HashSet a`
| `map<a,b>`  | `(Eq a, Ord a) => Map a b`
| `hash_map<a,b>` | `(Eq a, Hashable a) => HashMap a b`

## User Defined Types

### Structs/Exceptions

Thrift structs and exceptions are essentially the same. Both are generated as record types. The only difference is that exceptions get an `Exception` instance so that they can be thrown from RPCs.

If a struct field is marked as `optional`, then its Haskell type will be a `Maybe`.

**Thrift:**

```
struct User {
  1: string name,
  2: i64 id,
  3: optional string extra,
}
```

**Haskell:**

```haskell
data User = User
  { user_name  :: Text
  , user_id    :: Int64
  , user_extra :: Maybe Text
  }
```

### Enums

Enums become simple sum-types in Haskell where each constructor takes no arguments.

**Thrift:**

```
enum Color {
  Red = 0,
  Green = 1,
  Blue = 2,
}
```

**Haskell:**

```haskell
data Color = Color_Red | Color_Green | Color_Blue
```

(IMPORTANT)**A note on using `show` with Thrift enums:**
The behavior of `show` is derived by GHC, it is NOT specified in the Thrift standard. The implementation is not intended to be consistent with any sort of enum to string conversions in other languages and, if it is consistent, then that it purely by coincidence. The correct way to relate Thrift enum values across codebases is using one of the Thrift serialization methods (eg `serializeJSON`, `serializeBinary`, etc) or by converting to its numeric representation (`fromThriftEnum`).

### Unions

Unions are simple sum-types where each constructor takes a single value with the specified type.

Putting `(hs.nonempty)` after a union declaration will prevent generation of the `*_EMPTY` haskell data constructor in the Sum type. Example in D4037691.


**Thrift:**

```
struct Dog { ... }
struct Cat { ... }

union Pet {
  1: Dog dog,
  2: Cat cat,
}
```

**Haskell:**

```haskell
data Dog = Dog { ... }
data Cat = Cat { ... }

data Pet
  = Pet_dog Dog
  | Pet_cat Cat
```

### Typedefs

Thrift typedefs are just type synonyms in Haskell. It's also possible to generate a newtype instead by using an [[Dex/working-with-thrift-in-haskell/annotations | annotation]].

**Thrift:**

```
typedef i64 UserId
```

**Haskell:**

```haskell
type UserId = Int64
```
