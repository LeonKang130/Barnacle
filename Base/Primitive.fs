namespace Barnacle.Base

open System
open System.Numerics
open System.Runtime.CompilerServices
open System.Runtime.Intrinsics

[<IsReadOnly; Struct>]
type AxisAlignedBoundingBox =
    val pMin: Vector3
    val pMax: Vector3
    new(pMin: Vector3, pMax: Vector3) = { pMin = pMin; pMax = pMax }
    member inline this.Center = 0.5f * (this.pMin + this.pMax)
    member inline this.Diagonal = this.pMax - this.pMin

    member inline this.SurfaceArea =
        let diag = this.Diagonal
        2f * (diag.X * diag.Y + diag.Y * diag.Z + diag.Z * diag.X)

    member inline this.Volume =
        let diag = this.Diagonal
        diag.X * diag.Y * diag.Z
    member inline this.SplitAxis =
        let diag = this.Diagonal
        if diag.X >= diag.Y && diag.X >= diag.Z then 0
        elif diag.Y >= diag.Z then 1 else 2

    member inline this.Intersect(ray: Ray inref, t: float32) : bool =
        let invDir = Vector3.One / ray.Direction
        let mutable tMin = 1e-3f
        let mutable tMax = t
        let t0 = (this.pMin - ray.Origin) * invDir
        let t1 = (this.pMax - ray.Origin) * invDir
        let t0' = Vector3.Min(t0, t1)
        let t1' = Vector3.Max(t0, t1)
        tMin <- MathF.Max(tMin, MathF.Max(t0'.X, MathF.Max(t0'.Y, t0'.Z)))
        tMax <- MathF.Min(tMax, MathF.Min(t1'.X, MathF.Min(t1'.Y, t1'.Z)))
        tMin <= tMax

    static member inline Transform(aabb: AxisAlignedBoundingBox inref, m: Matrix4x4) =
        let mutable pMin, pMax = Vector3.PositiveInfinity, Vector3.NegativeInfinity

        for i = 0 to 7 do
            let pX = if i &&& 1 = 0 then aabb.pMin.X else aabb.pMax.X
            let pY = if i &&& 2 = 0 then aabb.pMin.Y else aabb.pMax.Y
            let pZ = if i &&& 4 = 0 then aabb.pMin.Z else aabb.pMax.Z
            let p = Vector3.Transform(Vector3(pX, pY, pZ), m)
            pMin <- Vector3.Min(pMin, p)
            pMax <- Vector3.Max(pMax, p)

        AxisAlignedBoundingBox(pMin, pMax)
    static member inline Transform(aabb: AxisAlignedBoundingBox, m: Matrix4x4) =
        AxisAlignedBoundingBox.Transform(&aabb, m)
    
    static member inline Union(a: AxisAlignedBoundingBox, b: AxisAlignedBoundingBox) =
        AxisAlignedBoundingBox(Vector3.Min(a.pMin, b.pMin), Vector3.Max(a.pMax, b.pMax))
    static member inline Union(a: AxisAlignedBoundingBox, b: Vector3) =
        AxisAlignedBoundingBox(Vector3.Min(a.pMin, b), Vector3.Max(a.pMax, b))
    static member Default = AxisAlignedBoundingBox(Vector3.PositiveInfinity, Vector3.NegativeInfinity)

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

    member inline this.EvalBSDF(wo: Vector3, wi: Vector3) : BSDFEval =
        let wo = this.geom.onb.WorldToLocal(wo)
        let wi = this.geom.onb.WorldToLocal(wi)
        this.Material.Eval(wo, wi, this.geom.uv)

    member inline this.SampleBSDF(wo: Vector3, uBSDF: Vector2) : BSDFSample =
        let sample = this.Material.Sample(this.geom.onb.WorldToLocal(wo), this.UV, uBSDF)
        { sample with wi = this.geom.onb.LocalToWorld(sample.wi) }

    member inline this.EvalEmit(wo: Vector3) : Vector3 =
        this.Light.Eval(this.geom.onb.WorldToLocal(wo), this.UV)
    
    member inline this.SampleEmit(uEmit: Vector2) : LightSample =
        let sample = this.Light.SampleEmit(uEmit)
        { eval = { sample.eval with p = this.Position }; wi = this.geom.onb.LocalToWorld(sample.wi) }

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

[<Sealed>]
type ListAggregate(instances: PrimitiveInstance array) =
    inherit PrimitiveAggregate()

    override this.Intersect(ray: Ray inref, t: float32) =
        let mutable hit = false
        let mutable instanceId = 0

        while not hit && instanceId < instances.Length do
            hit <- instances[instanceId].Intersect(&ray, t)
            instanceId <- instanceId + 1

        hit

    override this.Intersect(ray: Ray inref, interaction: Interaction outref, t: float32 byref) =
        let mutable hit = false

        for instance in instances do
            hit <- instance.Intersect(&ray, &interaction, &t) || hit

        hit
