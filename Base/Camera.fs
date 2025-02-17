namespace Barnacle.Base

open System.Numerics

[<AbstractClass>]
type CameraBase(pushForward: float32) =
    member val CameraToWorld = Unchecked.defaultof<Matrix4x4> with get, set
    member this.PushForward = pushForward
    abstract member GenerateRay: struct (int * int) * struct (int * int) * Vector2 * Vector2 -> struct (Ray * float32)
    member inline this.UpdateTransform(cameraToWorld: Matrix4x4) = this.CameraToWorld <- cameraToWorld

    member inline this.GeneratePrimaryRay(resolution: struct (int * int), pixelId: struct (int * int), uPixel: Vector2, uLens: Vector2) =
        let mutable struct (ray, pdf) = this.GenerateRay(resolution, pixelId, uPixel, uLens)
        ray.Origin <- Vector3.Transform(ray.Origin, this.CameraToWorld)
        ray.Direction <-
            Vector3.Transform(ray.Direction, this.CameraToWorld)
            - this.CameraToWorld.Translation
            |> Vector3.Normalize
        ray.Origin <- ray.PointAt this.PushForward
        struct (ray, pdf)
