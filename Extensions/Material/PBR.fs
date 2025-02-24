namespace Barnacle.Extensions.Material

open Barnacle.Base
open System
open System.Numerics

[<Sealed>]
type PBRMaterial(baseColor: Vector3, metallic: float32, roughness: float32) =
    inherit MaterialBase()
    member val BaseColor = baseColor with get
    member val Alpha = MathF.Max(roughness * roughness, 1e-3f) with get
    member val Metallic = Single.Clamp(metallic, 0f, 1f) with get
    member inline private this.Lambda(w: Vector3) =
        let sin2Theta = MathF.FusedMultiplyAdd(w.X, w.X, w.Y * w.Y)
        if sin2Theta = 0f then 0f
        else
            let tan2Theta = sin2Theta / (w.Z * w.Z)
            let alpha2Tan2Theta = this.Alpha * this.Alpha * tan2Theta
            (-1f + MathF.Sqrt(1f + alpha2Tan2Theta)) / 2f
    member inline private this.D(wh: Vector3) =
        let inline sqr (x: float32) = x * x
        let cosThetaH = MathF.Abs(wh.Z)
        this.Alpha * this.Alpha / (MathF.PI * sqr(1f + MathF.FusedMultiplyAdd(this.Alpha, this.Alpha, -1f) * cosThetaH * cosThetaH))
    member inline private this.G(wo: Vector3, wi: Vector3) =
        1f / (1f + this.Lambda(wo) + this.Lambda(wi))
    member inline private this.ConductorFresnel(wo: Vector3, wh: Vector3) =
        let cosTheta = Vector3.Dot(wo, wh)
        let c = 1f - cosTheta
        let c2 = c * c
        this.BaseColor + (Vector3.One - this.BaseColor) * (c2 * c2 * c)
    member inline private this.DielectricFresnel(wo: Vector3, wh: Vector3) =
        let f0 = 0.04f
        let cosTheta = Vector3.Dot(wo, wh)
        let c = 1f - cosTheta
        let c2 = c * c
        f0 + (1f - f0) * c2 * c2 * c
    override this.Eval(wo, wi, _): BSDFEval =
        let inline mix (a: float32, b: float32, t: float32) = MathF.FusedMultiplyAdd(a, 1f - t, b * t)
        let inline mix' (a: Vector3, b: Vector3, t: float32) = Vector3.FusedMultiplyAdd(a, Vector3(1f - t), b * t)
        let wh = Vector3.Normalize(wo + wi)
        let d = this.D(wh)
        let g = this.G(wo, wi)
        let specularBSDF = d * g / (4f * MathF.Abs(wo.Z))
        let metallicBSDF = specularBSDF * this.ConductorFresnel(wo, wh)
        let f = this.DielectricFresnel(wo, wh)
        let diffuseBSDF = this.BaseColor * MathF.Abs(wi.Z) / MathF.PI
        let bsdf = mix'(mix'(diffuseBSDF, Vector3(specularBSDF), f), metallicBSDF, this.Metallic)
        let pdf = mix(d * MathF.Abs(wh.Z) / (4f * Vector3.Dot(wo, wh)), MathF.Abs(wi.Z) / MathF.PI, 0.5f * (1f - this.Metallic))
        { bsdf = bsdf; pdf = pdf }
    override this.Sample(wo, uv, uLobe, uBSDF) =
        let wi =
            if uLobe < 1f - 0.5f * (1f - this.Metallic) then
                let thetaH = MathF.Atan(this.Alpha * MathF.Sqrt(uBSDF.X / (1f - uBSDF.X)))
                let phiH = 2f * MathF.PI * uBSDF.Y
                let struct (sinThetaH, cosThetaH) = MathF.SinCos(thetaH)
                let struct (sinPhiH, cosPhiH) = MathF.SinCos(phiH)
                let wh = Vector3(sinThetaH * cosPhiH, sinThetaH * sinPhiH, cosThetaH)
                2f * Vector3.Dot(wo, wh) * wh - wo
            else
                let cosTheta = MathF.Sqrt(uBSDF.X)
                let sinTheta = MathF.Sqrt(1f - uBSDF.X)
                let struct (sinPhi, cosPhi) = MathF.SinCos(2f * MathF.PI * uBSDF.Y)
                Vector3(sinTheta * cosPhi, sinTheta * sinPhi, cosTheta)
        { eval = this.Eval(wo, wi, uv); wi = wi }
    override this.IsDiffuse = true

