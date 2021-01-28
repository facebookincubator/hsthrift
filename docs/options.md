# Compiler Options

***Compiler options are used to globally modify some parameters across all Thrift files in the Typechecking tree***

## Global Type Modifiers

Similar to type annotations, there are a few compiler flags that allow you to globally modify types.


| Flag Name          | Description
| ------------------ | -----------
| `--use-int`        | Use `Int` instead of `Int64` (only safe on 64-bit machines)
| `--use-hash-map`   | Use `HashMap` instead of `Map`
| `--use-hash-set`   | Use `HashSet` instead of `Set`

## Required Symbols

Required symbols allows you to prune the Thrift AST before typechecking. This is useful to reduce compile times if you only want a few decls out of a giant thrift file (some Thrift files can take more than 10 minutes to compile). You can list the symbols from the Thrift file that you need, and the compiler will compute all the transitive dependencies and omit everything else from the generated code. To enable required symbols, use the following compiler flag with a comma-separated list of symbols to include.

```
--required-symbols = "X,Y,Z"
```

## Duplicate Names

Duplicate names (`--duplicate-names`) will generate un-prefixed struct fields. It uses the `DuplicateRecordFields` language extension so that the generated records can have fields with the same name.
