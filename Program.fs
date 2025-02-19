open Barnacle.Base
open Barnacle.Extensions.Camera
open Barnacle.Extensions.Primitive
open Barnacle.Extensions.Material
open Barnacle.Extensions.LightSampler
open Barnacle.Extensions.Aggregate
open Barnacle.Extensions.Integrator
open System
open System.Numerics
open System.Diagnostics

[<EntryPoint>]
let main _ =
    let imageWidth, imageHeight = 1024, 768
    let film = Film((imageWidth, imageHeight), ToneMapping.Aces)
    // let camera = PinholeCamera(30f, float32 imageWidth / float32 imageHeight, 140f)
    let camera = ThinLensCamera(4f, 220f, 25f, float32 imageWidth / float32 imageHeight, 140f)
    camera.UpdateTransform(Matrix4x4.CreateTranslation(50f, 40.8f, 295.6f))
    // Materials and lights
    let whiteMaterial = Lambertian(Vector3(0.75f)) :> MaterialBase
    let blackMaterial = Lambertian(Vector3.Zero) :> MaterialBase
    let redMaterial = Lambertian(Vector3(0.75f, 0.25f, 0.25f)) :> MaterialBase
    let blueMaterial = Lambertian(Vector3(0.25f, 0.25f, 0.75f)) :> MaterialBase
    let whiteLight = DiffuseLight(Vector3(6400f), false) :> LightBase
    // Primitives
    let floor = MeshPrimitive([| Vector3(1f, 0f, 0f); Vector3(99f, 0f, 0f); Vector3(99f, 0f, 181f); Vector3(1f, 0f, 181f) |], [| 0; 1; 2; 0; 2; 3 |])
    let ceiling = MeshPrimitive([| Vector3(1f, 81.6f, 0f); Vector3(99f, 81.6f, 0f); Vector3(99f, 81.6f, 181f); Vector3(1f, 81.6f, 181f) |], [| 0; 2; 1; 0; 3; 2 |])
    let leftWall = MeshPrimitive([| Vector3(1f, 0f, 0f); Vector3(1f, 81.6f, 0f); Vector3(1f, 81.6f, 181f); Vector3(1f, 0f, 181f) |], [| 0; 1; 2; 0; 2; 3 |])
    let rightWall = MeshPrimitive([| Vector3(99f, 0f, 0f); Vector3(99f, 81.6f, 0f); Vector3(99f, 81.6f, 181f); Vector3(99f, 0f, 181f) |], [| 0; 2; 1; 0; 3; 2 |])
    let frontWall = MeshPrimitive([| Vector3(1f, 0f, 181f); Vector3(99f, 0f, 181f); Vector3(99f, 81.6f, 181f); Vector3(1f, 81.6f, 181f) |], [| 0; 1; 2; 0; 3; 2 |])
    let backWall = MeshPrimitive([| Vector3(1f, 0f, 0f); Vector3(99f, 0f, 0f); Vector3(99f, 81.6f, 0f); Vector3(1f, 81.6f, 0f) |], [| 0; 2; 1; 0; 3; 2 |])
    let light = MeshPrimitive([| Vector3(-1f, 0f, 1f); Vector3(1f, 0f, 1f); Vector3(1f, 0f, -1f); Vector3(-1f, 0f, -1f) |], [| 0; 2; 1; 0; 3; 2 |])
    let bunny = MeshPrimitive.Load("Asset/stanford-bunny.obj")
    // Scene
    let node = Node(Transform(), [|
        Node(Transform(Matrix4x4.CreateScale(1f) * Matrix4x4.CreateTranslation(50f, 80f, 80f)), [| MeshInstance(light, whiteLight) :> PrimitiveInstance |])
        Node(Transform(Matrix4x4.CreateScale(16.5f) * Matrix4x4.CreateTranslation(27f, 16.5f, 47f)), [| SphereInstance(1f, whiteMaterial) :> PrimitiveInstance |])
        Node(Transform(Matrix4x4.CreateScale(200f) * Matrix4x4.CreateRotationY(Single.DegreesToRadians(30f)) * Matrix4x4.CreateTranslation(73f, -6.5f, 70f)), [| MeshInstance(bunny, whiteMaterial) :> PrimitiveInstance |])
    |], [|
        MeshInstance(leftWall, redMaterial)
        MeshInstance(rightWall, blueMaterial)
        MeshInstance(floor, whiteMaterial)
        MeshInstance(ceiling, whiteMaterial)
        MeshInstance(frontWall, blackMaterial)
        MeshInstance(backWall, whiteMaterial)
    |])
    let scene = Scene(node)
    let instances = scene.Traverse(0f)
    let aggregate = BVHAggregate(instances)
    let lightSampler = UniformLightSampler(instances)
    let integrator = PathTracingIntegrator(1024)
    let stopwatch = Stopwatch.StartNew()
    integrator.Render(camera, film, aggregate, lightSampler)
    stopwatch.Stop()
    printfn $"Render time: %f{stopwatch.Elapsed.TotalSeconds} seconds"
    film.Save "image.ppm"
    0