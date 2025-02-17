﻿namespace Barnacle.Extensions.Material

open Barnacle.Base
open System
open System.Numerics

[<Sealed>]
type Lambertian(albedo: Vector3) =
    inherit MaterialBase()
    member this.Albedo = albedo
    override this.Eval(wo: Vector3, wi: Vector3, _) =
        if wi.Z < 0f || wo.Z < 0f then
            { bsdf = Vector3.Zero; pdf = 0f }
        else
            let pdf = wi.Z / MathF.PI
            { bsdf = albedo * pdf; pdf = pdf }
    override this.Sample(_, _, uBSDF: Vector2) =
        let cosTheta = MathF.Sqrt(uBSDF.X)
        let sinTheta = MathF.Sqrt(1f - cosTheta * cosTheta)
        let struct (sinPhi, cosPhi) = MathF.SinCos(2f * MathF.PI * uBSDF.Y)
        let wi = Vector3(sinTheta * cosPhi, sinTheta * sinPhi, cosTheta)
        let pdf = wi.Z / MathF.PI
        { eval = { bsdf = albedo * pdf; pdf = pdf }; wi = wi }
