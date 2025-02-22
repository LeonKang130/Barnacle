namespace Barnacle.Extensions.Material

open Barnacle.Base
open System
open System.Numerics

// TODO: Add support for roughness
[<Sealed>]
type DielectricMaterial(ior: float32) =
    inherit MaterialBase()
    member this.IOR = ior
    override this.Eval(_, _, _) =
        { bsdf = Vector3.Zero; pdf = 0f }
    override this.Sample(wo, _, uBSDF) =
        let ior' = if wo.Z > 0f then 1f / ior else ior
        let cos2Theta = 1f - ior' * ior' * (1f - wo.Z * wo.Z)
        if cos2Theta <= 0f then
            { eval = { bsdf = Vector3.One; pdf = 1f }; wi = Vector3(-wo.X, -wo.Y, wo.Z) }
        else
            let cosTheta = MathF.Sqrt(cos2Theta)
            let a = ior' - 1f
            let b = ior' + 1f
            let r0 = a * a / (b * b)
            let c = 1f - cosTheta
            let r = r0 + (1f - r0) * c * c * c * c * c
            if uBSDF.X < r then
                { eval = { bsdf = Vector3(r); pdf = r }; wi = Vector3(-wo.X, -wo.Y, wo.Z) }
            else
                { eval = { bsdf = Vector3(1f - r); pdf = 1f - r }; wi = Vector3(-wo.X * ior', -wo.Y * ior', -MathF.CopySign(cosTheta, wo.Z)) }
    override this.IsDiffuse() = false
