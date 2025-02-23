namespace Barnacle.Extensions.Material

open Barnacle.Base
open System
open System.Numerics

// TODO: Add support for roughness
[<Sealed>]
type DielectricMaterial(baseColor: Vector3, ior: float32) =
    inherit MaterialBase()
    member val BaseColor = baseColor with get
    member val IOR = ior with get
    override this.Eval(_, _, _) =
        { bsdf = Vector3.Zero; pdf = 0f }
    override this.Sample(wo, _, uLobe, _) =
        let ior' = if wo.Z > 0f then 1f / this.IOR else this.IOR
        let cos2Theta = 1f - ior' * ior' * MathF.FusedMultiplyAdd(-wo.Z, wo.Z, 1f)
        if cos2Theta <= 0f then
            { eval = { bsdf = this.BaseColor; pdf = 1f }; wi = Vector3(-wo.X, -wo.Y, wo.Z) }
        else
            let a = ior' - 1f
            let b = ior' + 1f
            let r0 = a * a / (b * b)
            let c = 1f - MathF.Abs(wo.Z)
            let c2 = c * c
            let r = r0 + (1f - r0) * c2 * c2 * c
            if uLobe < r then
                { eval = { bsdf = r * this.BaseColor; pdf = r }; wi = Vector3(-wo.X, -wo.Y, wo.Z) }
            else
                let cosTheta = MathF.Sqrt(cos2Theta)
                { eval = { bsdf = (1f - r) * this.BaseColor; pdf = 1f - r }; wi = Vector3(-wo.X * ior', -wo.Y * ior', -MathF.CopySign(cosTheta, wo.Z)) }
    override this.IsDiffuse = false
