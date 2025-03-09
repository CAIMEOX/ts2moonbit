# MoonBit <=> TypeScript

## Primitive Types

Trivial

## Modeling Sub-typing

- Cast directly?

## Modeling Module

- MoonBit does not support cyclic dependency among packages
- Solution:
  1. Port all definitions to a single file / package (`internal`)
  2. Generate tree structure directory containing different "modules"
  3. Evert module (package) imports the `internal` package and defines type-alias
- Examples implementation:

```
├── internal
│   ├── internal.mbt
│   └── moon.pkg.json
├── mod1
│   ├── mod11
│   │   ├── mod11.mbt
│   │   └── moon.pkg.json
│   ├── mod12
│   │   ├── mod12.mbt
│   │   └── moon.pkg.json
│   ├── mod1.mbt
│   └── moon.pkg.json
└── moon.pkg.json
```
