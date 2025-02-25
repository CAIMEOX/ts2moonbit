module Targets.MoonBit.Target

open Ts2Ml
open Syntax

open Target
open Targets.MoonBit.Common
open Targets.MoonBit.Writer

open Fable.Core.JsInterop

let private builder (argv : Yargs.Argv<Options>) : Yargs.Argv<Options> = argv |> Options.register

let private run (input : Input) (ctx : IContext<Options>) =
  let outputDir =
    let curdir = Node.Api.``process``.cwd ()

    match ctx.options.outputDir with
    | None -> curdir
    | Some dir ->
      let path =
        if Node.Api.path.isAbsolute dir then
          dir
        else
          Node.Api.path.join [| curdir ; dir |]

      let fail () =
        failwithf "The output directory '%s' does not exist." path

      try
        if Node.Api.fs.lstatSync(!^path).isDirectory () then
          path
        else
          fail ()
      with _ ->
        fail ()

  ctx.logger.tracef "Output directory: %s" outputDir

let target =
  { new ITarget<Options> with
      member __.Command = "mbt"
      member __.Description = "Generate binding for MoonBit"
      member __.Builder = builder
      member __.Run (src, options) = run src options
  }
