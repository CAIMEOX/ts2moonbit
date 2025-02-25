ts2moonbit
========

Generate MoonBit bindings from TypeScript definitions via the TypeScript compiler API. 

- Forked from [ts2ocaml](https://github.com/ocsigen/ts2ocaml), still **working in progress**

## Roadmap

**Goals:**
* Provide a way to quickly generate a missing binding to some JS package you need in your project.
  - This tool can generate MoonBit bindings to JS packages from TS definition files (`.d.ts`), which you can use with minimal modification.
* Help library authors by reducing the amount of boilerplate code they have to write by hand.
  - This tool can be configured to generate an imperfect but simpler version of bindings, which you can easily modify to create a better binding library.

**Non-goals:**
* Generate a binding written in 100% MoonBit-idiomatic way.
* Perfectly replicate TypeScript's type system in MoonBit.

## Notes

For developers and contributors:
- [Overview for developers](docs/development.md)
- [Note on modeling TS subtyping in OCaml](docs/modeling_subtyping.md)

## About this tool

This tool is inspired by [ts2ocaml](https://github.com/ocsigen/ts2ocaml), which generates OCaml or ReScript bindings from TS definition files

Following the original project, this tools is licensed under the [Apache License 2.0](LICENSE.md).
