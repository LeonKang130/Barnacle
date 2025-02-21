namespace Barnacle.Extensions.Camera

open Barnacle.Base
open System
open System.Numerics

[<Sealed>]
type ThinLensCamera(aperture: float32, focusDistance: float32, fovY: float32, aspectRatio: float32, pushForward: float32) =
    inherit PinholeCamera(fovY, aspectRatio, pushForward)
    member this.Aperture = aperture
    member this.FocusDistance = focusDistance
    new(aperture: float32, focusDistance: float32, fovY: float32, aspectRatio: float32) =
        ThinLensCamera(aperture, focusDistance, fovY, aspectRatio, 0f)
    static member SampleDiskConcentric(uLens: Vector2) : Vector2 =
        let u = uLens * 2f - Vector2.One
        if u.X = 0f || u.Y = 0f then
            Vector2.Zero
        elif MathF.Abs(u.X) > MathF.Abs(u.Y) then
            let r = u.X
            let theta = MathF.PI / 4f * (u.Y / u.X)
            r * Vector2(MathF.Cos(theta), MathF.Sin(theta))
        else
            let r = u.Y
            let theta = MathF.PI / 2f - MathF.PI / 4f * (u.X / u.Y)
            r * Vector2(MathF.Cos(theta), MathF.Sin(theta))
           
    override this.GenerateRay(resolution, pixelId, uPixel, uLens) =
        let struct (ray, pdf) = base.GenerateRay(resolution, pixelId, uPixel, Vector2.Zero)
        if this.Aperture > 0f then
            let pLens = this.Aperture * ThinLensCamera.SampleDiskConcentric(uLens)
            let origin = ray.Origin + Vector3(pLens.X, pLens.Y, 0f)
            let direction =
                ray.PointAt(this.FocusDistance) - origin
                |> Vector3.Normalize
            Ray(origin, direction), pdf
        else
            ray, pdf
    
    override this.ToString() =
        $"ThinLensCamera(Aperture: {this.Aperture}, FocusDistance: {this.FocusDistance}, FovY: {this.FovY}, AspectRatio: {this.AspectRatio})"