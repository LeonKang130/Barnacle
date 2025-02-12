namespace Barnacle.Base

open System.Numerics

[<Struct>]
type Ray =
    val mutable Origin: Vector3
    val mutable Direction: Vector3
    new(origin: Vector3, direction: Vector3) = { Origin = origin; Direction = direction }
    member this.PointAt(t: float32) = this.Origin + t * this.Direction
    static member Transform(ray: Ray inref, m: Matrix4x4) =
        let origin = Vector3.Transform(ray.Origin, m)
        let direction = Vector3.Transform(ray.Direction, m) - m.Translation
        Ray(origin, direction)