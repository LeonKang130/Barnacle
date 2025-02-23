open Barnacle.Base
open Barnacle.Extensions.Camera
open Barnacle.Extensions.LightSampler
open Barnacle.Extensions.Aggregate
open Barnacle.Extensions.Integrator
open Barnacle.Extensions.Scene
open System.Runtime.CompilerServices
open System.Numerics
open System.Diagnostics
open CommandLine

[<IsReadOnly; Struct>]
type Options = {
    [<Option('i', "input", Required = true, HelpText = "json file containing scene information")>] 
    input: string
    [<Option('o', "output", Required = true, HelpText = "output filename")>]
    output: string
}

let inline RenderImage(scene: Scene, camera: CameraBase, film: Film, integrator: IntegratorBase, t: float32) =
    let instances = scene.Traverse(t)
    let aggregate = BVHAggregate(instances)
    let lightSampler = UniformLightSampler(instances)
    integrator.Render(camera, film, aggregate, lightSampler)

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
    let imageWidth, imageHeight = 1024, 768
    let film = Film(struct (imageWidth, imageHeight), ToneMapping.Aces)
    let camera = ThinLensCamera(4f, 220f, 25f, float32 imageWidth / float32 imageHeight, 140f)
    camera.UpdateTransform(Matrix4x4.CreateTranslation(50f, 40.8f, 295.6f))
    let integrator = PSSMLTIntegrator(1024)
    let stopwatch = Stopwatch.StartNew()
    RenderImage(scene, camera, film, integrator, 0f)
    stopwatch.Stop()
    printfn $"Render time: %f{stopwatch.Elapsed.TotalSeconds} seconds"
    film.Save output
    0