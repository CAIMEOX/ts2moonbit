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

let literalToIdentifier (ctx : Context) (l : Literal) : text =
  let formatString (s : string) =
    (s :> char seq)
    |> Seq.map (fun c -> if Char.isAlphabetOrDigit c then c else '_')
    |> Seq.toArray
    |> System.String

  let formatNumber (x : 'a) =
    string x
    |> String.replace "+" "plus"
    |> String.replace "-" "minus"
    |> String.replace "." "_"

  match l with
  | LString s ->
    match
      ctx
      |> Context.bindCurrentSourceInfo (fun i -> i.typeLiteralsMap |> Map.tryFind l)
    with
    | Some i ->
      if String.forall (Char.isAlphabetOrDigit >> not) s then
        tprintf "s%i" i
      else
        tprintf "s%i_%s" i (formatString s)
    | None -> failwithf "the literal '%s' is not found in the context" s
  | LInt i -> tprintf "n_%s" (formatNumber i)
  | LFloat l -> tprintf "n_%s" (formatNumber l)
  | LBool true -> str "b_true"
  | LBool false -> str "b_false"

let anonymousInterfaceModuleName (_ : Context) (info : AnonymousInterfaceInfo) =
  match info.origin.valueName, info.origin.argName with
  | _, Some s
  | Some s, None -> sprintf "%s%d" (Naming.toCase Naming.PascalCase s) info.id
  | _ -> sprintf "AnonymousInterface%d" info.id

// let emitEnum (flags: EmitTypeFlags) ctx (cases : Set<Choice<Enum * EnumCase * _, Literal>>)
let emitTypeImpl (flags : EmitTypeFlags) (overrideFunc : OverrideFunc) (ctx : Context) (ty : Type) = failwith "todo"

let emitUnion (flags : EmitTypeFlags) (overrideFunc : OverrideFunc) (ctx : Context) (u : UnionType) : text =
  if not flags.resolveUnion then
    u.types
    |> List.distinct
    |> List.map (emitTypeImpl (EmitTypeFlags.noExt flags) overrideFunc ctx)
    |> Type.union
  else if flags.external = External.Return true then
    let u = ResolvedUnion.checkNullOrUndefined u

    let rest =
      if List.isEmpty u.rest then
        Type.never
      else
        let t = Union { types = u.rest }
        emitTypeImpl (EmitTypeFlags.noExt flags) overrideFunc ctx t

    match u.hasNull, u.hasUndefined with
    | true, _
    | _, true -> Type.option rest
    | false, false -> rest
  else
    let u = ResolvedUnion.resolve ctx u

    let case name value =
      {|
        name = Choice1Of2 name
        value = value
        attr = None
      |}

    let treatEnum (cases) =
      let handleLiteral l attr ty =
        match l with
        | LString s ->
          Choice1Of2
            {|
              name = Choice1Of2 s
              value = None
              attr = attr
            |}
        | LInt i ->
          Choice1Of2
            {|
              name = Choice2Of2 i
              value = None
              attr = attr
            |}
        | LFloat _ -> Choice2Of2 (ty |? Type.float)
        | LBool _ -> Choice2Of2 (ty |? Type.boolean)

      let cases =
        List.distinct
          [
            for c in cases do
              match c with
              | Choice1Of2 (_, _, ty) ->
                let ty = emitTypeImpl (EmitTypeFlags.noExt flags) overrideFunc ctx ty
                yield Choice2Of2 ty
              | Choice2Of2 l -> yield handleLiteral l None None
          ]

      let cases, rest = List.splitChoice2 cases
      [ yield! rest ]

    let treatArray (ts : Set<Type>) =
      let elemT =
        let elemT =
          match Set.toList ts with
          | [ t ] -> t
          | ts -> Union { types = ts }

        emitTypeImpl (EmitTypeFlags.noExt flags) overrideFunc ctx elemT

      Type.app Type.array [ elemT ]

    let treatDUMany du =
      let types =
        du
        |> Map.toList
        |> List.collect (fun (_, cases) -> Map.toList cases)
        |> List.map (fun (_, t) -> t)

      types
      |> List.map (emitTypeImpl (EmitTypeFlags.noExt { flags with resolveUnion = false }) overrideFunc ctx)
      |> List.distinct

    let baseTypes =
      [
        if not (Set.isEmpty u.caseEnum) then
          yield! treatEnum u.caseEnum
        if not (Map.isEmpty u.discriminatedUnions) then
          yield! treatDUMany u.discriminatedUnions
        match u.caseArray with
        | Some ts -> yield treatArray ts
        | None -> ()
        for t in u.otherTypes do
          yield emitTypeImpl (EmitTypeFlags.noExt { flags with resolveUnion = false }) overrideFunc ctx t
      ]


    failwith "todo"

type StructuredTextItemBase<'TypeDefText, 'Binding, 'EnumCaseText> =
  /// Will always be emitted to the moon.pkg.json file.
  | ImportText of text
  | TypeDefText of 'TypeDefText
  | TypeAliasText of text
  | Comment of text
  | Binding of 'Binding
  | EnumCaseText of 'EnumCaseText

and StructuredTextItem =
  StructuredTextItemBase<
    TypeDefText,
    (OverloadRenamer -> CurrentScope -> Binding),
    {|
      name : string
      comments : Comment list
    |}
   >

and TypeDefText =
  {
    name : string
    tyargs : (TypeParam * text) list
    body : text option
    isRec : bool
    shouldAssert : bool
    attrs : text list
    comments : text list
  }

  static member Create (name, tyargs, body, ?attrs, ?comments, ?isRec, ?shouldAssert) =
    TypeDefText
      {
        name = name
        tyargs = tyargs
        body = body
        attrs = attrs |? []
        comments = comments |? []
        isRec = isRec |? false
        shouldAssert = shouldAssert |? false
      }

and CurrentScope =
  {
    jsModule : string option
    /// reversed list of scope
    scopeRev : string list
  }
