namespace Barnacle.Extensions.Material

open Barnacle.Base
open System
open System.Numerics

[<Sealed>]
type Lambertian(albedo: Vector3) =
    inherit MaterialBase()
    member this.Albedo = albedo

    override this.Eval(wo, wi, _) =
        if wi.Z * wo.Z < 0f || MathF.Min(MathF.Abs(wi.Z), MathF.Abs(wo.Z)) < 1e-6f then
            { bsdf = Vector3.Zero; pdf = 0f }
        else
            let pdf = MathF.Abs(wi.Z) / MathF.PI
            { bsdf = albedo * pdf; pdf = pdf }

    override this.Sample(wo, _, uBSDF) =
        let cosTheta = MathF.Sqrt(uBSDF.X)
        let sinTheta = MathF.Sqrt(1f - cosTheta * cosTheta)
        let struct (sinPhi, cosPhi) = MathF.SinCos(2f * MathF.PI * uBSDF.Y)
        let wi = Vector3(sinTheta * cosPhi, sinTheta * sinPhi, cosTheta)
        let pdf = wi.Z / MathF.PI

        { eval = { bsdf = albedo * pdf; pdf = pdf }
          wi = if wo.Z > 0f then wi else -wi }
    
    override this.IsDiffuse() = true

    override this.ToString() = $"Lambertian(Albedo: {this.Albedo})"
