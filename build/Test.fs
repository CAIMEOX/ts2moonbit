module Test

#nowarn "20" "52"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.JavaScript

let rootDir = Path.getFullName "."
let srcDir = "./src"
let outputDir = "./output"
let distDir = "./dist"
let testDir = "./test"

let inDirectory dirName action =
  Shell.cd dirName
  action ()
  Shell.cd rootDir

let run cmd dir args =
  let result =
    CreateProcess.fromRawCommandLine cmd args
    |> CreateProcess.withWorkingDirectory dir
    |> Proc.run

  if result.ExitCode <> 0 then
    failwithf "Error while running '%s' with args: %s " cmd args

let platformTool tool =
  lazy
    ProcessUtils.tryFindFileOnPath tool
    |> function
      | Some t -> t
      | _ -> failwithf "%s not found" tool

let dotnetExec cmd args =
  let result = DotNet.exec id cmd args

  if not result.OK then
    failwithf "Error while running 'dotnet %s %s'" cmd args

let moonTool = platformTool "moon"

let moon args = run moonTool.Value "./" args

// Build targets

let setup () =
  Target.create "Restore" <| fun _ -> DotNet.restore id "ts2ocaml.sln"

  Target.create "Prepare" ignore

  Target.create "BuildForTest"
  <| fun _ -> dotnetExec "fable" $"{srcDir} --sourceMaps --define DEBUG --run webpack --mode=development"

  Target.create "Build" ignore

  Target.create "Watch"
  <| fun _ -> dotnetExec "fable" $"watch {srcDir} --sourceMaps --define DEBUG --run webpack -w --mode=development"

  Target.create "Test" ignore

  "Restore" ==> "Prepare"

  "Prepare" ==> "BuildForTest" ==> "Build"

  "Prepare" ==> "Watch"

  "Restore"
  ?=> "Prepare"
  ?=> "BuildForTest"
  ?=> "Test"

// Test targets

module Test =
  module MoonBit =
    let testDir = testDir </> "moonbit"
    let outputDir = testDir </> "output"
    let srcDir = testDir </> "src"

    let clean () =
      !! $"{outputDir}/*" |> Seq.iter Shell.rm

    let generateBindings () =
      Directory.create outputDir

      let ts2mbt args files =
        Yarn.exec (sprintf "ts2ocaml mbt %s" (String.concat " " (Seq.append args files))) id

      ts2mbt [ "--verbose" ; $"-o {outputDir}" ] []

    let build () =
      for file in outputDir |> Shell.copyRecursiveTo true srcDir do
        printfn "* copied to %s" file

      inDirectory testDir <| fun () -> moon "build"

  let setup () =
    Target.create "TestMoonBitClean" <| fun _ -> MoonBit.clean ()

    Target.create "TestMoonBitGenerateBindings"
    <| fun _ -> MoonBit.generateBindings ()

    Target.create "TestMoonBitBuild" <| fun _ -> MoonBit.build ()
    Target.create "TestMoonBit" ignore

    "BuildForTest"
    ==> "TestMoonBitClean"
    ==> "TestMoonBitGenerateBindings"
    ==> "TestMoonBitBuild"
    ==> "TestMoonBit"
    ==> "Test"
// Utility targets

module Utility =
  let setup () =
    Target.create "UpdateBindings" <| fun _ -> BindingUpdater.run ()
    "Prepare" ==> "UpdateBindings"

[<EntryPoint>]
let main argv =
  // ensure working at the repository root
  Shell.cd (Path.combine __SOURCE_DIRECTORY__ "..")

  argv
  |> Array.toList
  |> Context.FakeExecutionContext.Create false "build.fsx"
  |> Context.RuntimeContext.Fake
  |> Context.setExecutionContext

  setup ()
  Test.setup ()
  Utility.setup ()

  Target.create "All" ignore
  "Prepare" ==> "Build" ==> "Test" ==> "All"

  // start build
  Target.runOrDefault "All"

  0
