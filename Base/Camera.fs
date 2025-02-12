namespace Barnacle.Base

open System.Numerics

[<AbstractClass>]
type CameraBase(pushForward: float32) =
    member val private _cameraToWorld = Matrix4x4.Identity with get, set
    member this.PushForward = pushForward
    abstract member GenerateRay: (int * int) * (int * int) * Vector2 * Vector2 -> (Ray * float32)
    member this.UpdateTransform(cameraToWorld: Matrix4x4) = this._cameraToWorld <- cameraToWorld

    member this.GeneratePrimaryRay(resolution: int * int, pixelId: int * int, uPixel: Vector2, uLens: Vector2) =
        let ray, pdf = this.GenerateRay(resolution, pixelId, uPixel, uLens)
        let origin = Vector3.Transform(ray.Origin, this._cameraToWorld)

        let direction =
            Vector3.Transform(ray.Direction, this._cameraToWorld)
            - this._cameraToWorld.Translation
            |> Vector3.Normalize

        Ray(origin + this.PushForward * direction, direction), pdf
