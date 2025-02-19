namespace Barnacle.Base

open System.Numerics

[<Struct>]
type Ray =
    val mutable Origin: Vector3
    val mutable Direction: Vector3
    new(origin: Vector3, direction: Vector3) = { Origin = origin; Direction = direction }
    member inline this.PointAt(t: float32) = Vector3.FusedMultiplyAdd(Vector3(t), this.Direction, this.Origin)
    static member inline Transform(ray: Ray inref, m: Matrix4x4) =
        let origin = Vector3.Transform(ray.Origin, m)
        let direction = Vector3.Transform(ray.Direction, m) - m.Translation
        Ray(origin, direction)
    static member inline Transform(ray: Ray, m: Matrix4x4) =
        Ray.Transform(&ray, m)