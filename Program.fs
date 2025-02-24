open Barnacle.Base
open Barnacle.Extensions.Scene
open System.Runtime.CompilerServices
open CommandLine

[<IsReadOnly; Struct>]
type Options = {
    [<Option('i', "input", Required = true, HelpText = "json file containing scene information")>] 
    input: string
    [<Option('o', "output", Required = true, HelpText = "output filename")>]
    output: string
}

[<EntryPoint>]
let main (args: string array) =
    let mutable input = Unchecked.defaultof<string>
    let mutable output = Unchecked.defaultof<string>
    match Parser.Default.ParseArguments<Options>(args) with
    | :? Parsed<Options> as parsed ->
        input <- parsed.Value.input
        output <- parsed.Value.output
    | _ ->
        failwith "Invalid arguments."
    let scene = Scene.Load input
    printfn $"Loaded scene from {input}"
    // TODO: Configure render time through scene file
    scene.Render(0f, output)
    0