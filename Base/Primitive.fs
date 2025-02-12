namespace Barnacle.Base

open System
open System.Numerics

[<Struct>]
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

        let b = Vector3.Cross(n, t)
        { n = n; t = t; b = b }

    new(n: Vector3, t: Vector3) =
        let b = Vector3.Cross(n, t)
        { n = n; t = t; b = b }

    member inline this.LocalToWorld(v: Vector3) =
        v.X * this.t + v.Y * this.b + v.Z * this.n

    member inline this.WorldToLocal(v: Vector3) =
        Vector3(Vector3.Dot(v, this.t), Vector3.Dot(v, this.b), Vector3.Dot(v, this.n))

    static member inline Transform(onb: OrthonormalBasis inref, m: Matrix4x4) =
        let t = Vector3.Transform(onb.t, m) - m.Translation |> Vector3.Normalize
        let b = Vector3.Transform(onb.b, m) - m.Translation |> Vector3.Normalize
        let n = Vector3.Cross(t, b)
        OrthonormalBasis(n, t, b)

[<Struct>]
type LocalGeometry =
    val mutable p: Vector3
    val mutable onb: OrthonormalBasis
    val mutable uv: Vector2
    val mutable tag: int // for primitives containing multiple sub-primitives, e.g. triangle meshes
    new(p: Vector3, onb: OrthonormalBasis, uv: Vector2, tag: int) = { p = p; onb = onb; uv = uv; tag = tag }
    new(p: Vector3, onb: OrthonormalBasis, uv: Vector2) = { p = p; onb = onb; uv = uv; tag = 0 }

    new(p: Vector3) = LocalGeometry(p, OrthonormalBasis(Vector3.UnitZ), Vector2.Zero)

    new(p: Vector3, n: Vector3) = LocalGeometry(p, OrthonormalBasis(n), Vector2.Zero)

    new(p: Vector3, n: Vector3, uv: Vector2) = LocalGeometry(p, OrthonormalBasis(n), uv)

    static member inline Transform(g: LocalGeometry inref, m: Matrix4x4) =
        LocalGeometry(Vector3.Transform(g.p, m), OrthonormalBasis.Transform(&g.onb, m), g.uv)

[<AbstractClass>]
type ElementalPrimitive() =
    abstract member Intersect: Ray inref * float32 -> bool
    abstract member Intersect: Ray inref * LocalGeometry byref * float32 byref -> bool

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

    member inline this.EvalBSDF(wo: Vector3, wi: Vector3) : BSDFEval =
        let wo = this.geom.onb.WorldToLocal(wo)
        let wi = this.geom.onb.WorldToLocal(wi)
        this.Material.Eval(wo, wi, this.geom.uv)

    member inline this.SampleBSDF(wo: Vector3, uBSDF: Vector2) : BSDFSample =
        let wo = this.geom.onb.WorldToLocal(wo)
        let mutable sample = this.Material.Sample(wo, this.UV, uBSDF)
        sample.wi <- this.geom.onb.LocalToWorld(sample.wi)
        sample
    
    member inline this.EvalEmit(wo: Vector3): Vector3 =
        let wo = this.geom.onb.WorldToLocal(wo)
        this.Light.Eval(wo, this.UV)

and [<AbstractClass>] PrimitiveInstance
    (primitive: ElementalPrimitive, objectToWorld: Matrix4x4, material: MaterialBase option, light: LightBase option) =
    let worldToObject =
        let mutable inv = Matrix4x4.Identity
        Matrix4x4.Invert(objectToWorld, &inv) |> ignore
        inv

    member this.Primitive = primitive
    member this.ObjectToWorld = objectToWorld
    member this.WorldToObject = worldToObject
    member this.HasMaterial = material.IsSome
    member this.Material = material.Value
    member this.HasLight = light.IsSome
    member this.Light = light.Value

    member inline this.Intersect(ray: Ray inref, t: float32) =
        let ray = Ray.Transform(&ray, this.WorldToObject)
        this.Primitive.Intersect(&ray, t)

    member inline this.Intersect(ray: Ray inref, interaction: Interaction byref, t: float32 byref) =
        let ray = Ray.Transform(&ray, this.WorldToObject)
        let mutable geom = Unchecked.defaultof<LocalGeometry>

        if this.Primitive.Intersect(&ray, &geom, &t) then
            interaction.geom <- LocalGeometry.Transform(&geom, this.ObjectToWorld)
            interaction.inst <- this
            true
        else
            false

    abstract member Sample: Vector2 -> Interaction * float32
    abstract member EvalPDF: Interaction inref -> float32

[<AbstractClass>]
type PrimitiveAggregate(instances: PrimitiveInstance[]) =
    member this.Instances = instances
    abstract member Intersect: Ray inref * float32 -> bool
    abstract member Intersect: Ray inref * Interaction byref * float32 byref -> bool

type ListAggregate(instances: PrimitiveInstance[]) =
    inherit PrimitiveAggregate(instances)

    override this.Intersect(ray: Ray inref, t: float32) =
        let mutable hit = false
        let mutable instanceId = 0

        while not hit && instanceId < this.Instances.Length do
            hit <- this.Instances[instanceId].Intersect(&ray, t) || hit
            instanceId <- instanceId + 1

        hit

    override this.Intersect(ray: Ray inref, interaction: Interaction byref, t: float32 byref) =
        let mutable hit = false

        for instance in this.Instances do
            hit <- instance.Intersect(&ray, &interaction, &t) || hit

        hit
