namespace Barnacle.Base
open System.Numerics

[<Struct>]
type BSDFEval = {
    mutable bsdf: Vector3
    mutable pdf: float32
}

[<Struct>]
type BSDFSample = {
    mutable eval: BSDFEval
    mutable wi: Vector3
}

[<AbstractClass>]
type MaterialBase() =
    abstract member Eval: Vector3 * Vector3 * Vector2 -> BSDFEval
    abstract member Sample: Vector3 * Vector2 * Vector2 -> BSDFSample
    abstract member IsDiffuse: unit -> bool
