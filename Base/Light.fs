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
    abstract member Eval: Vector3 * Vector2 -> Vector3

type DiffuseLight(emission: Vector3, twoSided: bool) =
    inherit LightBase(twoSided)
    new(emission: Vector3) = DiffuseLight(emission, true)
    member this.Emission = emission

    override this.Eval(wo: Vector3, _) =
        if wo.Z > 1e-6f || (this.TwoSided && wo.Z < -1e-6f) then
            this.Emission
        else
            Vector3.Zero