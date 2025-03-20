module Targets.MoonBit.Common

open Ts2Ml
open DataTypes


type Options =
    inherit GlobalOptions
    inherit Typer.TyperOptions

    abstract debug: bool with get
    abstract outputDir: string option with get

module Options =
    open Fable.Core.JsInterop

    let register (yargs: Yargs.Argv<Options>) =
        yargs
            .addFlag(
                "debug",
                (fun (o: Options) -> o.debug),
                descr = "Enable debug mode (For development purpose).",
                defaultValue = false
            )
            .addOption (
                "output-dir",
                (fun (o: Options) -> o.outputDir),
                descr = "The directory to place the generated bindings.\nIf not set, it will be the current directory.",
                alias = "o"
            )

type Output = {
  fileName: string
  content: text
  stubLines: string list
}