﻿namespace Barnacle.Extensions.Primitive

open Barnacle.Base
open System
open System.Numerics

[<Sealed>]
type SpherePrimitive(radius: float32) =
    inherit ElementalPrimitive()
    new() = SpherePrimitive(1f)
    member this.Radius = radius

    override this.Intersect(ray: Ray inref, t: float32) =
        let eps = 1e-3f
        let f = ray.Origin
        let a = ray.Direction.LengthSquared()
        let b = - Vector3.Dot(f, ray.Direction)
        let r2 = this.Radius * this.Radius
        let c = f.LengthSquared() - r2
        let d = r2 - (f + b / a * ray.Direction).LengthSquared()

        if d < 0f then
            false
        else
            let q = b + MathF.CopySign(MathF.Sqrt(a * d), b)
            let t0 = c / q

            if t0 > eps && t0 < t then
                true
            else
                let t1 = q / a

                t1 > eps && t1 < t

    override this.Intersect(ray: Ray inref, geom: LocalGeometry outref, t: float32 byref) =
        let eps = 1e-3f
        let f = ray.Origin
        let a = ray.Direction.LengthSquared()
        let b = - Vector3.Dot(f, ray.Direction)
        let r2 = this.Radius * this.Radius
        let c = f.LengthSquared() - r2
        let d = r2 - (f + b / a * ray.Direction).LengthSquared()

        if d < 0f then
            false
        else
            let q = b + MathF.CopySign(MathF.Sqrt(a * d), b)
            let t0 = c / q

            if t0 > eps && t0 < t then
                t <- t0
                let p = ray.PointAt t
                let mutable n = Vector3.Normalize(p)

                let uv =
                    Vector2(MathF.Atan2(n.Z, n.X) / (2f * MathF.PI) + 0.5f, MathF.Acos(n.Y) / MathF.PI)

                if Vector3.Dot(n, ray.Direction) > 0f then
                    n <- -n

                geom <- LocalGeometry(p, n, uv)
                true
            else
                let t1 = q / a

                if t1 > eps && t1 < t then
                    t <- t1
                    let p = ray.PointAt t
                    let mutable n = Vector3.Normalize(p)

                    let uv =
                        Vector2(MathF.Atan2(n.Z, n.X) / (2f * MathF.PI) + 0.5f, MathF.Acos(n.Y) / MathF.PI)

                    if Vector3.Dot(n, ray.Direction) > 0f then
                        n <- -n

                    geom <- LocalGeometry(p, n, uv)
                    true
                else
                    false
    
    override this.Bounds = AxisAlignedBoundingBox(Vector3(-this.Radius), Vector3(this.Radius))

[<Sealed>]
type SphereInstance(sphere: SpherePrimitive, material: MaterialBase option, light: LightBase option) =
    inherit PrimitiveInstance(sphere, material, light)

    new(radius: float32, material: MaterialBase, light: LightBase) =
        SphereInstance(SpherePrimitive(radius), Some material, Some light)

    new(radius: float32, material: MaterialBase) =
        SphereInstance(SpherePrimitive(radius), Some material, None)

    new(radius: float32, light: LightBase) =
        SphereInstance(SpherePrimitive(radius), None, Some light)

    member this.Sphere = sphere

    override this.Sample(_, uSurface: Vector2) =
        let struct (sinTheta, cosTheta) = MathF.SinCos(2f * MathF.PI * uSurface.X)
        let cosPhi = 1f - 2f * uSurface.Y
        let sinPhi = MathF.Sqrt(1f - cosPhi * cosPhi)

        let n =
            Vector3(cosTheta * sinPhi, sinTheta * sinPhi, cosPhi)

        let mutable interaction = Unchecked.defaultof<Interaction>
        interaction.geom <- LocalGeometry(n * this.Sphere.Radius, n, uSurface)
        interaction.inst <- this
        let p' = Vector3.Transform(interaction.Position, this.ObjectToWorld)

        let t' =
            Vector3.Transform(interaction.Tangent, this.ObjectToWorld)
            - this.ObjectToWorld.Translation

        let b' =
            Vector3.Transform(interaction.Bitangent, this.ObjectToWorld)
            - this.ObjectToWorld.Translation

        let n' = Vector3.Cross(t', b')
        let invJacobian = 1f / n'.Length()
        interaction.geom <- LocalGeometry(p', invJacobian * n', uSurface)
        interaction, invJacobian / (4f * MathF.PI * this.Sphere.Radius * this.Sphere.Radius)

    override this.EvalPDF(interaction: Interaction inref) =
        let t' =
            Vector3.Transform(interaction.Tangent, this.WorldToObject)
            - this.WorldToObject.Translation

        let b' =
            Vector3.Transform(interaction.Bitangent, this.WorldToObject)
            - this.WorldToObject.Translation

        let n' = Vector3.Cross(t', b')
        let invJacobian = n'.Length()
        invJacobian / (4f * MathF.PI * this.Sphere.Radius * this.Sphere.Radius)
