module Targets.MoonBit.MoonBitHelper

open System
open Ts2Ml
open Syntax
open DataTypes

open DataTypes.Text

let comment text =
  if text = empty then
    empty
  else
    let inner =
      if isMultiLine text then
        prependNewline (str "///") (indent text) + newline
      else
        between " " " " text

    inner

let commentStr text = tprintf "/* %s */" text

module Naming =
  let removeInvalidChars (s : string) =
    s.Trim('"').ToCharArray ()
    |> Array.map (fun c ->
      if Char.isAlphabetOrDigit c || c = '_' || c = '\'' then
        c
      else
        '_'
    )
    |> System.String

  let keywords =
    set
      [
        "as"
        "else"
        "enum"
        "for"
        "if"
        "match"
        "struct"
        "type"
        "while"
        "loop"
      ]

  let reservedNames = set [ "export" ; "default" ; "types" ]

  let upperFirst (s : string) =
    if Char.IsLower s[0] then
      sprintf "%c%s" (Char.ToUpper s[0]) s[1..]
    else
      s

let lowerFirst (s : string) =
  if Char.IsUpper s[0] then
    sprintf "%c%s" (Char.ToLower s[0]) s[1..]
  else
    s

module Type =
  let tuple =
    function
    | [] -> failwith "empty tuple"
    | _ :: [] -> failwith "unary tuple"
    | xs -> concat (str ",") xs |> between "(" ")"



  let arrow args ret =
    let lhs =
      match args with
      | [] -> str "()"
      | xs -> concat (str ", ") xs |> between "(" ")"

    lhs +@ " -> " + ret

  let app t args =
    match args with
    | [] -> failwith "type application with empty arguments"
    | _ -> t + between "[" "]" (concat (str ", ") args)


  let rec union =
    function
    | [] -> failwith "union type with zero elements"
    | x :: [] -> x
    | xs -> app (tprintf "@js.Union%i" (List.length xs)) xs

  // TODO: add intersection type to js header
  let rec intersection =
    function
    | [] -> failwith "intersection type with zero elements"
    | x :: [] -> x
    | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: rest ->
      app (str "Intersection.t8") [ x1 ; x2 ; x3 ; x4 ; x5 ; x6 ; x7 ; intersection (x8 :: rest) ]
    | xs -> app (tprintf "@js.Intersection%i" (List.length xs)) xs

  // primitive types
  let unit = str "Unit"
  let string = str "String"
  let boolean = str "Bool"
  let int = str "Int"
  let float = str "Float"
  let double = str "Double"
  let number = double

  let array = str "Array"
  let option t = app (str "Option") [ t ]


  // typescript types
  let never = str "Never"
  let any = str "@js.Value"
  let object = str "@js.Object"
  let undefined = unit
  let null_ = str "@js.Nullable<never>"
  let nullable t = app (str "@js.Nullable") [ t ]

[<RequireQualifiedAccess>]
module Term =
  let tuple =
    function
    | [] -> failwith "empty tuple"
    | _ :: [] -> failwith "unary tuple"
    | xs -> concat (str ", ") xs |> between "(" ")"

  let app t us =
    t + (us |> concat (str ", ") |> between "(" ")")

  let literal =
    function
    | LBool true -> str "true"
    | LBool false -> str "false"
    | LInt i -> i |> string |> str
    | LFloat f -> tprintf "%f" f
    | LString s -> tprintf "\"%s\"" (String.escape s)

// TODO: well, MoonBit's module system, hmm
type TextModule =
  {|
    name : string
    origName : string
    content : text list
    comments : text list
  |}

module Statement =
  let let_ name typ value =
    tprintf "let %s: " name + typ +@ " = " + value

  let typeAlias name ty_args (typ : text option) =
    let lhs =
      str "typealias "
      + if List.isEmpty ty_args then
          str name
        else
          Type.app (str name) ty_args

    match typ with
    | None -> lhs
    | Some ty -> lhs +@ " = " + ty

  let extType name ty_args =
    str "type "
    + if List.isEmpty ty_args then
        str name
      else
        Type.app (str name) ty_args

  // TODO: check result is valid js identifier
  let external name (typ : text) target =
    let result = tprintf "extern \"js\" fn %s%s = %s" name (string typ) target
    result

[<RequireQualifiedAccess>]
type Binding =
  | Let of
    {|
      name : string
      ty : text
      body : text
      attrs : text list
      comments : text list
    |}
  | Ext of
    {|
      name : string
      ty : text
      target : string
      attrs : text list
      comments : text list
    |}
  | Unknown of
    {|
      msg : text option
      comments : text list
    |}

  member this.comments =
    match this with
    | Let x -> x.comments
    | Ext x -> x.comments
    | Unknown x -> x.comments

module Binding =
  let let_ (attrs : text list) comments name ty body =
    Binding.Let
      {|
        name = name
        ty = ty
        body = body
        attrs = attrs
        comments = comments
      |}

  let ext (attrs : text list) comments name ty target =
    Binding.Ext
      {|
        name = name
        ty = ty
        target = target
        attrs = attrs
        comments = comments
      |}

  let unknown comments msg =
    Binding.Unknown {| msg = msg ; comments = comments |}

  let cast comments name ty =
    Binding.Ext
      {|
        name = name
        ty = ty
        target = "%identity"
        attrs = []
        comments = comments
      |}
