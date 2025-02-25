module Targets.MoonBit.MoonBitHelper

open System
open Ts2Ml
open Syntax
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
    let removeInvalidChars (s: string) =
        s.Trim('"').ToCharArray()
        |> Array.map (fun c ->
            if Char.isAlphabetOrDigit c || c = '_' || c = '\'' then
                c
            else
                '_')
        |> System.String

    let keywords =
        set
            [ "as"
              "else"
              "enum"
              "for"
              "if"
              "match"
              "struct"
              "type"
              "while"
              "loop" ]

    let reservedNames = set [ "export"; "default"; "types" ]

    let upperFirst (s: string) =
        if Char.IsLower s[0] then
            sprintf "%c%s" (Char.ToUpper s[0]) s[1..]
        else
            s

  let lowerFirst (s: string) =
      if Char.IsUpper s[0] then
          sprintf "%c%s" (Char.ToLower s[0]) s[1..]
      else
          s
