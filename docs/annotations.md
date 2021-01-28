# Annotations

### Customize the generated Haskell code my modifying particular structures and types

## Splice files (`hs_include`)

A thrift file can have a super special include statement `hs_include` such as:

```
hs_include "sigma/haxl/si/apps/tally/if/tally_config.hs"
```

This will cause the Haskell thrift compiler to splice that file into the file of generated types.  This allows you to add import statements and class instances, to allow non-orphan instances of the new haskell types.

## Type Annotations

Type annotations are used to transform the type of some value in the generated Haskell code. This does *not* affect the way that the value is serialized, so it still maintains compatibility with other languages.

| Annotated Type | Haskell Type
| -------------- | ------------
| `i64 (hs.type = "Int")` | `Int`
| `string (hs.type = "String")` | `String`
| `list<a> (hs.type = "Vector")` | `Data.Vector.Vector a`
| `list<a> (hs.type = "VectorStorable")` | `Data.Vector.Storable.Vector a`

## Prefix Annotations

Prefix annotations are used to make struct field names nicer. The default field prefix is the struct's de-capitalized name followed by an underscore. For example, consider this struct:

```
struct X {
  1: string name;
}
```

The field will be called `x_name`, however, if we add an annotation, we can change the name:

```
struct X {
  1: string name;
} (hs.prefix = "the_")
```

Now the field will be called `the_name`. We can even use `(hs.prefix = "")` to remove the prefix altogether.

The prefix annotation also works for enums and unions.

## Union enums

Putting `(hs.nonempty)` after a union declaration will prevent generation of the `*_EMPTY` haskell data constructor in the Sum type.

## Pseudo Enums

Very large enums (several thousand constructors) lead to Haskell code that is slow to compile. The pseudo-enum annotation instead generates a newtype with named constants for the enum values.

**Thrift**

```
enum X {
  A = 0,
  B = 1,
  C = 2,
}

enum Y {
  A = 0,
  B = 1,
  C = 2,
} (hs.psuedoenum)
```

**Haskell**

```haskell
data X = X_A | X_B | X_C | X_UNKNOWN Int32

newtype Y = Y Int32

y_a, y_b, y_c :: Y
y_a = Y 0
y_b = Y 1
y_c = Y 2
```

## No Unknown constructor Enums

In Haskell, enums generate an unknown constructor to handle unknown enum values. This  feature can be turned off for a single enum using the `(hs.nounknown)` annotation.

```
enum X {
  A = 0,
  B = 1,
  C = 2,
} (hs.nounknown)
```

## Newtypes

By including the `(hs.newtype)` annotation on a Thrift typedef, you can create a Haskell newtype instead of a simple type synonym.

**Thrift:**

```
typedef string Name
typedef i64 Id (hs.newtype)
```

**Haskell:**

```haskell
type Name = Text
newtype Id = Id { unId :: Int }
```

Unlike type synonyms, newtype creates a new, distinct type. Thus, adding newtype annotations affects the typechecking rules in Thrift. Named constants must be assigned values with matching types. For example:

```
typedef i64 Id (hs.newtype)

const Id  a = 1 // allowed, because 1 is a literal of type Id
const Id  b = a // allowed, because 'a' and 'b' both have type Id
const i64 c = a // not allowed, because the types of 'a' and 'c' differ

const i64 c = 2 // allowed
const Id  d = c // not allowed
```
