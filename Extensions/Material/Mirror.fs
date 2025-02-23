namespace Barnacle.Extensions.Material

open Barnacle.Base
open System.Numerics

// TODO: Add support for roughness
[<Sealed>]
type MirrorMaterial(baseColor: Vector3) =
    inherit MaterialBase()
    member val BaseColor = baseColor with get
    override this.Eval(_, _, _) =
        { bsdf = Vector3.Zero; pdf = 0f }
    override this.Sample(wo, _, _, _) =
        { eval = { bsdf = this.BaseColor; pdf = 1f }; wi = Vector3(-wo.X, -wo.Y, wo.Z) }
    override this.IsDiffuse = false
