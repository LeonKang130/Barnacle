namespace Barnacle.Base

open System.Numerics
    
[<AbstractClass>]
type LightSamplerBase(primitiveInstances: PrimitiveInstance array) =
    let instances = primitiveInstances |> Array.filter (_.HasLight)
    do if instances.Length = 0 then
        failwith "LightSamplerBase: No light primitives found."
    member this.Instances = instances
    abstract member Sample: Vector3 * float32 * Vector2 -> LightSample
    abstract member Eval: Vector3 * Interaction inref -> LightEval

