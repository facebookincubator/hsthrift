# Plan

We're going to migrate the Thrift compiler from using old-style
unstructured annotations to structured annotations. Examples:

Currently:

```
typedef map<string, string> hsnewtypeann (hs.newtype)
```

Using structured annotations:

```
@haskell.Newtype
typedef map<string, string> hsnewtypeann
```

Currently:

```
struct HsStruct {
  1: i32 strictann (hs.strict);
  2: i32 lazyann (hs.lazy);
  3: i32 inherit;
}
```

Using structured annotations:

```
struct HsStruct {
  @Haskell.Strict
  1: i32 strictann;
  @Haskell.Lazy
  2: i32 lazyann;
  3: i32 inherit;
}
```

## Current status

The compiler already parses and typechecks structured annotations, but
the Haskell-specific annotations like `hs.strict`, `hs.newtype`
etc. are all unstructured. The compiler doesn't currently have
structured equivalents for these annotations.

We need to change the typechecker to allow structured annotations to
be used for these Haskell-specific annotations.

## Requirements

* Modify the compiler to allow structured annotations to be used for
Haskell-specific annotations - that is, all annotations beginning `hs.`.

* Do all required error checking.

* Add appropriate tests, to mirror the existing tests under
  `compiler/test` but using structured annotations instead. Ensure
  `cabal test thrift-compiler` passes.

* Finally, convert the tests under tests/ (the thrift-tests package)
  to use structured annotations, and ensure that `cabal test
  thrift-tests` passes.

## Resources

* We'll be working without fbthrift, I have set the flag `-fbthrift`
  in `cabal.project`.

* I have copied the upstream canonical structured annotations files
  into `thrift/annotations`, and added the beginnings of a
  `haskell.thrift` file for the Haskell annotations.

## Remaining TODOs

### Support `hs.type` / `haskell.Type`

The `hs.type` annotation allows overriding the generated Haskell type
for a typedef. Supporting a structured `@haskell.Type` equivalent would
require architectural changes to pass structured annotations through
type resolution at the `AnnotatedType` level.

### Convert remaining test files to structured annotations

`hs_test.thrift` and `service.thrift` in `tests/if/` were not converted
because adding `include "haskell.thrift"` brings in dependency modules
(e.g. `Scope/Types.hs`, `Haskell/Types.hs`) whose generated output
differs when `--extra-hasfields` is used, causing fixture conflicts with
existing expected output.

## Completed

* `filterHiddenFields` moved from the parser into the typechecker
  (`resolveField` in `Typechecker.hs`), now uses resolved annotations
  via `hasResolvedAnn "Hidden"`.

* `isHaskellAnn` now checks `namePackage` against the canonical package
  URI `"facebook.com/thrift/annotation/haskell"` instead of matching
  the file path suffix. `Name` has a new `namePackage :: Maybe Text`
  field, populated from the `package` declaration in the source thrift
  file.

* `hs.prefix` on structs: `renameField` now takes both unstructured
  annotations and `[StructuredAnnotation 'Resolved l Loc]`, and the
  plugin handles prefix extraction. The typechecker passes annotations
  through without interpreting them.

* `renameFunction`, `renameEnumAlt`, `renameUnionAlt`, `getUnionEmptyName`
  all take resolved structured annotations. The Haskell plugin checks
  `getResolvedPrefix` first, falling back to unstructured `getPrefix`.

* All map builders (`mkConstMap`, `mkEnumMap`, `mkUnionMap`,
  `mkServiceMap`) now resolve structured annotations before calling
  rename methods. `mkSchemaMap` is built first (sequentially) so its
  result is available to the others.

* `getStructuredPrefix` removed — all prefix handling now goes through
  `getResolvedPrefix` on resolved annotations.

* `enumFlavourTag` now takes `[StructuredAnnotation 'Resolved l Loc]`
  and checks `hasResolvedAnn`/`getResolvedAnnStringField` for
  `PseudoEnum` and `NoUnknown`, falling back to unstructured
  `hs.pseudoenum`/`hs.nounknown`. Both call sites updated:
  `resolveEnum` passes already-resolved annotations, and `mkTypemap`
  resolves annotations via `runTypechecker` (works because annotation
  types come from imports).

* `getDeclIface` passes `[]` for resolved annotations — it runs before
  typechecking (for declaration pruning) so resolved annotations are not
  available. This is acceptable because symbol prefixes and enum flavour
  don't affect pruning decisions. A comment in the code documents this.
