# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

- ReScript: prefix invalid constructor names by `Case` (#392).

## [2.0.0-beta.2]

- Update and fix TypeScript version to `5.3.3`.

## [2.0.0-beta.1]

- ReScript: changed the standard library from `Js` to `Core` (#387).
  - `ArrayLike` and `Iterable` are still mapped to `Js.Array2.array_like` since it doesn't seem to be available in `Core` yet.

## [2.0.0-beta.0]

- Add ReScript as a target (#32).
  - Add a new subcommand `ts2ocaml res`.
  - The documentation is available on [`docs/rescript.md`](docs/rescript.md).

## [2.0.0-alpha.0]

- Upgrade TypeScript to v5.
- Added an explicit support of ambient modules.
  - Topological sorting inside ambient modules now works as expected (#157).
- Perform various improvements over messages.
  - Messages now come with color (warning: yellow, error: red).
  - The error location is now shown with a code frame.
- Deprecate the `--safe-arity` option.
  - Ts2ocaml now generates minimal arity-safe output by default.
- Perform massive internal refactoring.

## [1.4.6] - 2023-07-13

- Fix a bug which generated unnecessarily duplicated option type (#315).

## [1.4.5] - 2023-07-13

- Fix a bug which caused optional properties not to be recognized as optional (#312).

## [1.4.4] - 2022-05-09

- Fix a bug which caused ts2ocaml to crash when encountering an optional field with type `null | undefined`.

## [1.4.3] - 2022-05-05

- Ts2ocaml now fails when it encounters unknown options, instead of just ignoring it.

## [1.4.2] - 2022-03-16

- Fix a bug which caused ts2ocaml to generate invalid type declarations.
- Fix a bug which causes ts2ocaml to crash when loading mutually-referencing source files.

## [1.4.1] - 2022-03-11

- Fix a bug which prevented ts2ocaml from working on Windows environment.

## [1.4.0] - 2022-03-04

- Upgrade js_of_ocaml to 4.0 and dune to 3.0.
- Perform massive refactoring on the generated bindings **(breaking changes)**.
  - Anonymous interface modules are now generated in the module where they are actually used.
  - Changed the representation of untagged union types and intersection types to make it easier to use.
    - `('a, 'b) and_` and `('a, 'b) or_` types are removed in this change.
  - Union types appearing as argument of function are now emitted in a simpler form: `` [`U1 of t1 | `U2 of t2 | .. ] [@js.union] ``.
    - Now you don't have to do `Union.inject_n` on function arguments.
  - Union of primitive types are now represented as `[...] Primitive.t`.
    - Use `Primitive.classify` function to convert it to a polymorphic variant, on which you can `match` directly.
    - `'a or_XXX` types are removed in this change.
- Ts2ocaml now emits builder function `[@js.builder]` for POJO interfaces.
- Add an option `--readable-names` to try to use more readable names instead of `AnonymousInterfaceN`.
- Fix a bug which prevented ts2ocaml from generating class constructors if not defined explicitly.

## [1.3.1] - 2021-12-24

- Fix a bug which prevented ts2ocaml from handling enum types without explicit values.
- Fix a bug which prevented ts2ocaml from parsing package.json correctly.

## [1.3.0] - 2021-12-10

- Upgrade gen_js_api to 1.0.9.
- Ts2ocaml now merges enum case names if they have the same value.
  - This is because gen_js_api now profibits enum cases with duplicate values.
  - This is a breaking change.

```typescript
enum Foo {
  A = 1,
  B = 1,
  C = 2,
}
```

```ocaml
module Foo: sig
  (* before *)
  type t = [ `A [@js 1] | `B [@js 2] | `C [@js 2] ] [@js.enum]

  (* after  *)
  type t = [ `A_B [@js 1] | `C [@js 2] ] [@js.enum]
end
```

## [1.2.1] - 2021-12-07

- Fix a bug which "relativized" the path of the specified output directory.
- Fix a bug which prevented ts2ocaml from parsing package.json correctly.

## [1.2.0] - 2021-11-30

- Add an option `--merge` to merge the input definition files to one binding.
  - This is a breaking change; previously `--merge` was the default behavior.
- Add an option `--follow-relative-references` to generate bindings for relevant files at once.

## [1.1.0] - 2021-11-24

- Upgrade and fixed TypeScript version to >= 4.5.2 < 4.6.0.
- Fix the problem which prevented `--create-minimal-stdlib` from working correctly.
- Add better heuristic for relative imports.
- Fix bugs on relative imports.

## [1.0.0] - 2021-11-08

Official release. Nothing is changed internally.

## [0.0.5] - 2021-11-08

Test automated publishing for the official release. Nothing is changed internally.

## [0.0.4] - 2021-11-08

Test automated publishing for the official release. Nothing is changed internally.

## [0.0.3] - 2021-11-02

Nothing is changed internally, but the GitHub Action is now working as intended.
Publishing the NPM package and the OPAM package (to `jsoo-stdlib` branch for OPAM pinning) is now automated.

## [0.0.2] - 2021-11-02

Test if package is published correctly when we create an release on GitHub.
Also create a branch to be used as the standard library for js_of_ocaml.

## [0.0.1] - 2021-10-22

Test publishing to npm.

## [0.0.0] - 2021-10-21

In development.
