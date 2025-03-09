module Targets.MoonBit.Writer

open Ts2Ml
open Syntax
open Typer
open Typer.Type
open DataTypes
open DataTypes.Text

open Targets.MoonBit.Common
open Targets.MoonBit.MoonBitHelper

type ScriptTarget = TypeScript.Ts.ScriptTarget

type State =
  {|
    fileNames : string list
    info : Result<PackageInfo, string option>
    referencesCache : MutableMap<string list, WeakTrie<string>>
  |}


module State =
  let create fileNames info : State =
    {|
      fileNames = fileNames
      info = info
      referencesCache = new MutableMap<_, _> ()
    |}

type Context = TyperContext<Options, State>
type TypeEmitter = Context -> Type -> text

[<RequireQualifiedAccess>]
type External =
  | Root of variadic : bool * nullable : bool
  | Return of nullable : bool
  | Argument of variadic : bool
  | None

type EmitTypeFlags =
  {
    resolveUnion : bool
    needParen : bool
    external : External
    avoidTheseArgumentNames : Set<string>
  }

module EmitTypeFlags =
  let defaultValue =
    {
      resolveUnion = true
      needParen = false
      external = External.None
      avoidTheseArgumentNames = Set.empty
    }

  let noExt f = { f with external = External.None }

  let ofFuncArg (variadic : bool) f =
    { f with
        external =
          match f.external with
          | External.Root _ -> External.Argument variadic
          | _ -> External.None
    }

  let ofFuncReturn f =
    { f with
        external =
          match f.external with
          | External.Root (_, n) -> External.Return n
          | _ -> External.None
    }

module Context = TyperContext

let fixme alter fmt =
  Printf.ksprintf (fun msg -> commentStr (sprintf "FIXME: %s" msg) + alter) fmt

type OverrideFunc = EmitTypeFlags -> TypeEmitter -> Context -> Type -> text option

module OverrideFunc =
  let inline noOverride _f _et _ctx _ty = None

  let inline combine (f1 : OverrideFunc) (f2 : OverrideFunc) : OverrideFunc =
    fun f et ctx ty ->
      match f2 f et ctx ty with
      | Some t -> Some t
      | None -> f1 f et ctx ty
