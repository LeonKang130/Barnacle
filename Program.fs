﻿open Barnacle.Base
open Barnacle.Extensions.Camera
open Barnacle.Extensions.Primitive
open Barnacle.Extensions.Material
open Barnacle.Extensions.LightSampler
open Barnacle.Extensions.Aggregate
open Barnacle.Extensions.Integrator
open System.Numerics

[<EntryPoint>]
let main _ =
    let imageWidth, imageHeight = 1024, 768
    let mutable film = Film((imageWidth, imageHeight), ToneMapping.Aces)
    // let mutable camera = PinholeCamera(30f, float32 imageWidth / float32 imageHeight, 140f)
    let mutable camera = ThinLensCamera(10f, 215f, 30f, float32 imageWidth / float32 imageHeight, 140f)
    camera.UpdateTransform(Matrix4x4.CreateTranslation(50f, 40.8f, 295.6f))
    let whiteMaterial = Lambertian(Vector3(0.75f)) :> MaterialBase
    let blackMaterial = Lambertian(Vector3.Zero) :> MaterialBase
    let redMaterial = Lambertian(Vector3(0.75f, 0.25f, 0.25f)) :> MaterialBase
    let blueMaterial = Lambertian(Vector3(0.25f, 0.25f, 0.75f)) :> MaterialBase
    let whiteLight = DiffuseLight(Vector3(8f), true) :> LightBase
    let mutable node = Node(Transform(), [|
        Node(Transform(Matrix4x4.CreateTranslation(1e3f + 1f, 40.8f, 81.6f)), [| SphereInstance(1e3f, redMaterial) :> PrimitiveInstance |])
        Node(Transform(Matrix4x4.CreateTranslation(-1e3f + 99f, 40.8f, 81.6f)), [||], [| SphereInstance(1e3f, blueMaterial) :> PrimitiveInstance |])
        Node(Transform(Matrix4x4.CreateTranslation(50f, 40.8f, 1e3f)), [| SphereInstance(1e3f, whiteMaterial) :> PrimitiveInstance |])
        Node(Transform(Matrix4x4.CreateTranslation(50f, 1e3f, 81.6f)), [| SphereInstance(1e3f, whiteMaterial) :> PrimitiveInstance |])
        Node(Transform(Matrix4x4.CreateTranslation(50f, -1e3f + 81.6f, 81.6f)), [| SphereInstance(1e3f, whiteMaterial) :> PrimitiveInstance |])
        Node(Transform(Matrix4x4.CreateTranslation(50f, 40.8f, -1e3f + 181f)), [| SphereInstance(1e3f, blackMaterial) :> PrimitiveInstance |])
        Node(Transform(Matrix4x4.CreateScale(16.5f) * Matrix4x4.CreateTranslation(27f, 16.5f, 47f)), [| SphereInstance(1f, whiteLight) :> PrimitiveInstance |])
        Node(Transform(Matrix4x4.CreateScale(16.5f) * Matrix4x4.CreateTranslation(73f, 16.5f, 78f)), [| SphereInstance(1f, whiteMaterial) :> PrimitiveInstance |])
    |])
    let mutable scene = Scene(node)
    let instances = scene.Traverse(0f)
    let aggregate = BVHAggregate(instances)
    let lightSampler = UniformLightSampler(instances)
    let integrator = PathTracingIntegrator(256)
    integrator.Render(camera, film, aggregate, lightSampler)
    film.Save "image.ppm"
    0