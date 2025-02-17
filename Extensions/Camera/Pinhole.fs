namespace Barnacle.Extensions.Camera

open Barnacle.Base
open System
open System.Numerics

type PinholeCamera(fovY: float32, aspectRatio: float32, pushForward: float32) =
    inherit CameraBase(pushForward)
    member this.FovY = fovY
    member this.AspectRatio = aspectRatio
    new(fovY: float32, aspectRatio: float32) = PinholeCamera(fovY, aspectRatio, 0f)

    override this.GenerateRay(resolution: struct (int * int), pixelId: struct (int * int), uPixel: Vector2, _) =
        let struct (imageWidth, imageHeight) = resolution
        let struct (imageX, imageY) = pixelId
        let viewportHeight = 2f * MathF.Tan(fovY * MathF.PI / 360f)
        let viewportWidth = viewportHeight * aspectRatio
        let viewportU = viewportWidth * Vector3.UnitX
        let viewportV = viewportHeight * Vector3.UnitY
        let pixelDeltaU = viewportU / float32 imageWidth
        let pixelDeltaV = viewportV / float32 imageHeight
        let viewportUpperLeft = -Vector3.UnitZ - 0.5f * (viewportU + viewportV)

        let pixelLocation =
            viewportUpperLeft
            + (float32 imageX + uPixel.X) * pixelDeltaU
            + (float32 imageY + uPixel.Y) * pixelDeltaV
        Ray(Vector3.Zero, Vector3.Normalize(pixelLocation)), 1f
