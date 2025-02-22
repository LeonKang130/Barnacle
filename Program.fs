open Barnacle.Base
open Barnacle.Extensions.Camera
open Barnacle.Extensions.Primitive
open Barnacle.Extensions.Material
open Barnacle.Extensions.LightSampler
open Barnacle.Extensions.Aggregate
open Barnacle.Extensions.Integrator
open Barnacle.Extensions.Scene
open System
open System.Numerics
open System.Diagnostics
open CommandLine

type options = {
    [<Option('i', "input", Required = true)>] 
    input: string
    [<Option('o', "output", Required = true)>]
    output: string
}

[<EntryPoint>]
let main (args: string array) =
    let mutable input = Unchecked.defaultof<string>
    let mutable output = Unchecked.defaultof<string>
    match Parser.Default.ParseArguments<options>(args) with
    | :? Parsed<options> as parsed ->
        input <- parsed.Value.input
        output <- parsed.Value.output
    | _ -> 
        failwith "Invalid arguments."
    let imageWidth, imageHeight = 1024, 768
    let film = Film(struct (imageWidth, imageHeight), ToneMapping.Aces)
    // let camera = PinholeCamera(30f, float32 imageWidth / float32 imageHeight, 140f)
    let camera = ThinLensCamera(4f, 220f, 25f, float32 imageWidth / float32 imageHeight, 140f)
    camera.UpdateTransform(Matrix4x4.CreateTranslation(50f, 40.8f, 295.6f))
    let scene = Scene.Load input
    printfn $"Loaded scene from {input}"
    let instances = scene.Traverse(0f)
    let aggregate = BVHAggregate(instances)
    let lightSampler = UniformLightSampler(instances)
    let integrator = PathTracingIntegrator(16)
    let stopwatch = Stopwatch.StartNew()
    integrator.Render(camera, film, aggregate, lightSampler)
    stopwatch.Stop()
    printfn $"Render time: %f{stopwatch.Elapsed.TotalSeconds} seconds"
    film.Save output
    0