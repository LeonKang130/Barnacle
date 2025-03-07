﻿namespace Barnacle.Base

open Barnacle.Util
open System
open System.Numerics
open System.Runtime.CompilerServices

[<IsReadOnly; Struct>]
type OrthonormalBasis =
    val n: Vector3
    val t: Vector3
    val b: Vector3
    new(n: Vector3, t: Vector3, b: Vector3) = { n = n; t = t; b = b }

    new(n: Vector3) =
        let t =
            if MathF.Abs n.X > 0.1f then
                Vector3.UnitY
            else
                Vector3.UnitX
            |> fun x -> Vector3.Cross(n, x)
            |> Vector3.Normalize
        { n = n; t = t; b = Vector3.Cross(n, t) }

    new(n: Vector3, t: Vector3) =
        { n = n; t = t; b = Vector3.Cross(n, t) }

    member inline this.LocalToWorld(v: Vector3) =
        v.X * this.t + v.Y * this.b + v.Z * this.n

    member inline this.WorldToLocal(v: Vector3) =
        Vector3(Vector3.Dot(v, this.t), Vector3.Dot(v, this.b), Vector3.Dot(v, this.n))

    static member inline Transform(onb: OrthonormalBasis inref, m: Matrix4x4) =
        let t = Vector3.Transform(onb.t, m) - m.Translation |> Vector3.Normalize
        let b = Vector3.Transform(onb.b, m) - m.Translation |> Vector3.Normalize
        let n = Vector3.Cross(t, b)
        OrthonormalBasis(n, t, b)
    static member inline Transform(onb: OrthonormalBasis, m: Matrix4x4) =
        OrthonormalBasis.Transform(&onb, m)

[<Struct>]
type LocalGeometry =
    val p: Vector3
    val onb: OrthonormalBasis
    val uv: Vector2
    val mutable tag: int // for primitives containing multiple sub-primitives, e.g. triangle meshes
    new(p: Vector3, onb: OrthonormalBasis, uv: Vector2, tag: int) = { p = p; onb = onb; uv = uv; tag = tag }
    new(p: Vector3, onb: OrthonormalBasis, uv: Vector2) = { p = p; onb = onb; uv = uv; tag = 0 }

    new(p: Vector3) = LocalGeometry(p, OrthonormalBasis(Vector3.UnitZ), Vector2.Zero)

    new(p: Vector3, n: Vector3) = LocalGeometry(p, OrthonormalBasis(n), Vector2.Zero)

    new(p: Vector3, n: Vector3, uv: Vector2) = LocalGeometry(p, OrthonormalBasis(n), uv)

    static member inline Transform(g: LocalGeometry inref, m: Matrix4x4) =
        LocalGeometry(Vector3.Transform(g.p, m), OrthonormalBasis.Transform(&g.onb, m), g.uv)
    static member inline Transform(g: LocalGeometry, m: Matrix4x4) =
        LocalGeometry.Transform(&g, m)

[<AbstractClass>]
type ElementalPrimitive() =
    abstract member Intersect: Ray inref * float32 -> bool
    abstract member Intersect: Ray inref * LocalGeometry outref * float32 byref -> bool
    abstract member Bounds: AxisAlignedBoundingBox

[<Struct>]
type Interaction =
    val mutable geom: LocalGeometry
    val mutable inst: PrimitiveInstance
    member this.Position = this.geom.p
    member this.Normal = this.geom.onb.n
    member this.Tangent = this.geom.onb.t
    member this.Bitangent = this.geom.onb.b
    member this.UV = this.geom.uv
    member this.HasMaterial: bool = this.inst.HasMaterial
    member this.Material: MaterialBase = this.inst.Material
    member this.HasLight: bool = this.inst.HasLight
    member this.Light: LightBase = this.inst.Light
    member inline this.SpawnRay(wo: Vector3) = Ray(this.Position, wo)

    member inline this.EvalBSDF(wo: Vector3, wi: Vector3) =
        this.Material.Eval(this.geom.onb.WorldToLocal(wo), this.geom.onb.WorldToLocal(wi), this.geom.uv)

    member inline this.SampleBSDF(wo: Vector3, uLobe: float32, uBSDF: Vector2) =
        let mutable sample = this.Material.Sample(this.geom.onb.WorldToLocal(wo), this.UV, uLobe, uBSDF)
        sample.wi <- this.geom.onb.LocalToWorld(sample.wi)
        sample

    member inline this.EvalEmit(wo: Vector3) : Vector3 =
        this.Light.Eval(this.geom.onb.WorldToLocal(wo), this.UV)
    
    member inline this.SampleEmit(uEmit: Vector2) =
        let mutable sample = this.Light.SampleEmit(uEmit)
        sample.eval.p <- this.Position
        sample.wi <- this.geom.onb.LocalToWorld(sample.wi)
        sample

and [<AbstractClass>] PrimitiveInstance
    (primitive: ElementalPrimitive, material: MaterialBase option, light: LightBase option) =
    member this.Primitive = primitive
    member val ObjectToWorld = Unchecked.defaultof<Matrix4x4> with get, set
    member val WorldToObject = Unchecked.defaultof<Matrix4x4> with get, set
    member val Bounds = Unchecked.defaultof<AxisAlignedBoundingBox> with get, set
    member this.HasMaterial = material.IsSome
    member this.Material = material.Value
    member this.HasLight = light.IsSome
    member this.Light = light.Value

    member inline this.Intersect(ray: Ray inref, t: float32) =
        if this.Bounds.Intersect(&ray, t) then
            let ray = Ray.Transform(&ray, this.WorldToObject)
            this.Primitive.Intersect(&ray, t)
        else
            false

    member inline this.Intersect(ray: Ray inref, interaction: Interaction outref, t: float32 byref) =
        if this.Bounds.Intersect(&ray, t) then
            let ray = Ray.Transform(&ray, this.WorldToObject)

            if this.Primitive.Intersect(&ray, &interaction.geom, &t) then
                interaction.geom <- LocalGeometry.Transform(&interaction.geom, this.ObjectToWorld)
                interaction.inst <- this
                true
            else
                false
        else
            false

    member inline this.UpdateTransform(objectToWorld: Matrix4x4) =
        this.ObjectToWorld <- objectToWorld
        this.WorldToObject <-
            let mutable inv = Matrix4x4.Identity
            Matrix4x4.Invert(objectToWorld, &inv) |> ignore
            inv
        this.Bounds <-
            AxisAlignedBoundingBox.Transform(this.Primitive.Bounds, objectToWorld)
    
    abstract member Sample: float32 * Vector2 -> struct (Interaction * float32)
    abstract member EvalPDF: Interaction inref -> float32

[<AbstractClass>]
type PrimitiveAggregate() =
    abstract member Intersect: Ray inref * float32 -> bool
    abstract member Intersect: Ray inref * Interaction outref * float32 byref -> bool
    abstract member Bounds: AxisAlignedBoundingBox