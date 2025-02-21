namespace Barnacle.Base

open System
open System.Numerics

[<Struct>]
type LightEval = {
    mutable p: Vector3
    mutable L: Vector3
    mutable pdf: float32
}
    
[<Struct>]
type LightSample = {
    mutable eval: LightEval
    mutable wi: Vector3
}

[<AbstractClass>]
type LightBase(twoSided: bool) =
    member this.TwoSided = twoSided
    abstract member SampleEmit: Vector2 -> LightSample
    abstract member Eval: Vector3 * Vector2 -> Vector3

[<Sealed>]
type DiffuseLight(emission: Vector3, twoSided: bool) =
    inherit LightBase(twoSided)
    new(emission: Vector3) = DiffuseLight(emission, true)
    member this.Emission = emission

    override this.SampleEmit(uEmit: Vector2) =
        let inline sampleCosineHemisphere(u: Vector2) =
            let cosTheta = MathF.Sqrt(u.X)
            let sinTheta = MathF.Sqrt(1f - cosTheta * cosTheta)
            let struct (sinPhi, cosPhi) = MathF.SinCos(2f * MathF.PI * u.Y)
            Vector3(sinTheta * cosPhi, sinTheta * sinPhi, cosTheta)
        let mutable wo = Vector3.UnitZ
        let mutable pdf = 0f
        if this.TwoSided then
            if uEmit.X > 0.5f then
                wo <- -sampleCosineHemisphere(Vector2(2f * uEmit.X - 1f, uEmit.Y))
            else
                wo <- sampleCosineHemisphere(Vector2(2f * uEmit.X, uEmit.Y))
        else
            wo <- sampleCosineHemisphere(uEmit)
        pdf <- MathF.Abs(wo.Z) / MathF.PI * if this.TwoSided then 0.5f else 1f
        { eval = { p = Vector3.Zero; L = this.Emission; pdf = pdf }; wi = -wo }
    
    override this.Eval(wo: Vector3, _) =
        if MathF.Abs(wo.Z) > 1e-6f && (wo.Z > 0f || this.TwoSided) then
            this.Emission
        else
            Vector3.Zero
    
    override this.ToString() = $"DiffuseLight({this.Emission})"