namespace Barnacle.Base

open Barnacle.Util
open System
open System.Numerics

[<Struct>]
type Ray =
    val mutable Origin: Vector3
    val mutable Direction: Vector3

    new(origin: Vector3, direction: Vector3) =
        { Origin = origin
          Direction = direction }

    member inline this.PointAt(t: float32) =
        Vector3.FusedMultiplyAdd(Vector3(t), this.Direction, this.Origin)

    static member inline Transform(ray: Ray inref, m: Matrix4x4) =
        let origin = Vector3.Transform(ray.Origin, m)
        let direction = Vector3.Transform(ray.Direction, m) - m.Translation
        Ray(origin, direction)

    static member inline Transform(ray: Ray, m: Matrix4x4) = Ray.Transform(&ray, m)

[<AutoOpen>]
module BVHExtensions =
    type AxisAlignedBoundingBox with
        member inline this.Intersect(ray: Ray inref, t: float32) : bool =
            let invDir = Vector3.One / ray.Direction
            let mutable tMin = 1e-3f
            let mutable tMax = t
            let t0 = (this.pMin - ray.Origin) * invDir
            let t1 = (this.pMax - ray.Origin) * invDir
            let t0' = Vector3.MinNative(t0, t1)
            let t1' = Vector3.MaxNative(t0, t1)
            tMin <- Math.Max(tMin, Math.Max(t0'.X, Math.Max(t0'.Y, t0'.Z)))
            tMax <- Math.Min(tMax, Math.Min(t1'.X, Math.Min(t1'.Y, t1'.Z)))
            tMin <= tMax
