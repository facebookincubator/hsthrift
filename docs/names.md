# Names

### How thrift names translate into Haskell names.

Haskell requires all constants to be lower case, and all type and data constructors to be upper case. To conform with this, all type names (structs, unions, enum, etc) are upper cased and all constants are lower cased.

The names of struct fields in the Haskell record are created by appending the Thrift field name to the struct name with the first letter lowercased.  For example the field `bar` of a struct called `Foo` will have the name `foo_bar` in Haskell.

You can also specify a namespace in your thrift file for Haskell code generation:

```
namespace hs My.Module
```
