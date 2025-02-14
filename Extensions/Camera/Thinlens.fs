namespace Barnacle.Extensions.Camera

open Barnacle.Base
open System
open System.Numerics

type ThinlensCamera(aperture: float32, focalLength: float32, focusDistance: float32, pushForward: float32) =
    inherit CameraBase(pushForward)
    member this.Aperture = aperture
    member this.FocalLength = focalLength
    member this.FocusDistance = focusDistance
    new(aperture: float32, focalLength: float32, focusDistance: float32) =
        ThinlensCamera(aperture, focalLength, focusDistance, 0f)
    
    member this.Sample (uLens: Vector2) : Vector2 =
        let u = uLens * 2f - Vector2.One
        let p =
            if abs u.X > abs u.Y then
                let r = u.X
                let theta = MathF.PI / 4f * (u.Y / u.X)
                r * Vector2(MathF.Cos(theta), MathF.Sin(theta))
            else
                let r = u.Y
                let theta = MathF.PI / 2f - MathF.PI / 4f * (u.X / u.Y)
                r * Vector2(MathF.Cos(theta), MathF.Sin(theta))
        p
           
    override this.GenerateRay(resolution: int * int, pixelId: int * int, uPixel: Vector2, uLens: Vector2) =
        let resolutionVec = Vector2(float32 (fst resolution), float32 (snd resolution))
        let pixelIdVec = Vector2(float32 (fst pixelId), float32 (snd pixelId))
        let imageRatio = (1000f * focusDistance / focalLength) - 1f
        let lensRadius = 0.5f * focalLength / (aperture * 1000f)
        let pixelOffset = 0.5f * resolutionVec
        let pixelSize = 
            if resolutionVec.X > resolutionVec.Y then 
                min (imageRatio * 0.036f / resolutionVec.X) (imageRatio * 0.024f / resolutionVec.X)
            else 
                min (imageRatio * 0.024f / resolutionVec.X) (imageRatio * 0.036f / resolutionVec.Y)

        let coodFocal = (pixelIdVec - pixelOffset) * pixelSize
        let coodLens = this.Sample(uLens) * lensRadius
        let pixelFocal = Vector3(coodFocal.X, -coodFocal.Y, -focusDistance)
        let pixelLens = Vector3(coodLens.X, coodLens.Y, 0f)

        Ray(pixelLens, Vector3.Normalize(pixelFocal - pixelLens)), 1f