namespace Barnacle.Extensions.Primitive

open Barnacle.Base
open System
open System.Runtime.CompilerServices
open System.Numerics

[<Struct; IsReadOnly>]
type Vertex =
    val p: Vector3
    val n: Vector3
    val uv: Vector2
    new(p: Vector3, n: Vector3, uv: Vector2) = { p = p; n = n; uv = uv }
    static member Transform(vertex: Vertex inref, m: Matrix4x4) =
        let p = Vector3.Transform(vertex.p, m)
        let n = Vector3.TransformNormal(vertex.n, m)
        let uv = vertex.uv
        Vertex(p, n, uv)
    static member Transform(vertex: Vertex, m: Matrix4x4) =
        let p = Vector3.Transform(vertex.p, m)
        let n = Vector3.TransformNormal(vertex.n, m)
        let uv = vertex.uv
        Vertex(p, n, uv)

type TrianglePrimitive(v0: Vertex, v1: Vertex, v2: Vertex) =
    inherit ElementalPrimitive()
    new(p0: Vector3, p1: Vector3, p2: Vector3) =
        let n = Vector3.Normalize(Vector3.Cross(p1 - p0, p2 - p0))
        let v0 = Vertex(p0, n, Vector2.Zero)
        let v1 = Vertex(p1, n, Vector2(1f, 0f))
        let v2 = Vertex(p2, n, Vector2(0f, 1f))
        TrianglePrimitive(v0, v1, v2)
    member this.V0 = v0
    member this.V1 = v1
    member this.V2 = v2
    member this.E0 = v1.p - v0.p
    member this.E1 = v2.p - v0.p
    override this.Bounds =
        let pMin = Vector3.Min(Vector3.Min(v0.p, v1.p), v2.p)
        let pMax = Vector3.Max(Vector3.Max(v0.p, v1.p), v2.p)
        AxisAlignedBoundingBox(pMin, pMax)
    override this.Intersect(ray: Ray inref, t: float32) =
        let eps = Single.Epsilon
        let e0 = this.E0
        let e1 = this.E1
        let rayCrossE1 = Vector3.Cross(ray.Direction, e1)
        let det = Vector3.Dot(e0, rayCrossE1)
        if MathF.Abs(det) < eps then
            false
        else
            let invDet = 1f / det
            let s = ray.Origin - v0.p
            let u = invDet * Vector3.Dot(s, rayCrossE1)
            if u < 0f || u > 1f then
                false
            else
                let sCrossE0 = Vector3.Cross(s, e0)
                let v = invDet * Vector3.Dot(ray.Direction, sCrossE0)
                if v < 0f || u + v > 1f then
                    false
                else
                    let t' = invDet * Vector3.Dot(e1, sCrossE0)
                    t' > eps && t' < t
    override this.Intersect(ray: Ray inref, geom: LocalGeometry outref, t: float32 byref) =
        let eps = Single.Epsilon
        let e0 = this.E0
        let e1 = this.E1
        let rayCrossE1 = Vector3.Cross(ray.Direction, e1)
        let det = Vector3.Dot(e0, rayCrossE1)
        if MathF.Abs(det) < eps then
            false
        else
            let invDet = 1f / det
            let s = ray.Origin - v0.p
            let u = invDet * Vector3.Dot(s, rayCrossE1)
            if u < 0f || u > 1f then
                false
            else
                let sCrossE0 = Vector3.Cross(s, e0)
                let v = invDet * Vector3.Dot(ray.Direction, sCrossE0)
                if v < 0f || u + v > 1f then
                    false
                else
                    let t' = invDet * Vector3.Dot(e1, sCrossE0)
                    if t' > eps && t' < t then
                        t <- t'
                        let p = ray.PointAt t'
                        let n = Vector3.Normalize((1f - u - v) * v0.n + u * v1.n + v * v2.n)
                        let uv = (1f - u - v) * v0.uv + u * v1.uv + v * v2.uv
                        geom <- LocalGeometry(p, n, uv)
                        true
                    else
                        false

type TriangleInstance(triangle: TrianglePrimitive, material: MaterialBase option, light: LightBase option) =
    inherit PrimitiveInstance(triangle, material, light)
    new(triangle: TrianglePrimitive, material: MaterialBase) =
        TriangleInstance(triangle, Some material, None)
    new(triangle: TrianglePrimitive, light: LightBase) =
        TriangleInstance(triangle, None, Some light)
    member this.Primitive = triangle
    override this.Sample(uSurface: Vector2) =
        let uv =
            if uSurface.X < uSurface.Y then
                Vector2(0.5f * uSurface.X, -0.5f * uSurface.X + uSurface.Y)
            else
                Vector2(-0.5f * uSurface.Y + uSurface.X, 0.5f * uSurface.Y)
        let v0 = Vertex.Transform(this.Primitive.V0, this.ObjectToWorld)
        let v1 = Vertex.Transform(this.Primitive.V1, this.ObjectToWorld)
        let v2 = Vertex.Transform(this.Primitive.V2, this.ObjectToWorld)
        let p = (1f - uv.X - uv.Y) * v0.p + uv.X * v1.p + uv.Y * v2.p
        let n = Vector3.Normalize((1f - uv.X - uv.Y) * v0.n + uv.X * v1.n + uv.Y * v2.n)
        let uv = (1f - uv.X - uv.Y) * v0.uv + uv.X * v1.uv + uv.Y * v2.uv
        let pdf = 2f / Vector3.Cross(v1.p - v0.p, v2.p - v0.p).Length()
        let mutable interaction = Unchecked.defaultof<Interaction>
        interaction.geom <- LocalGeometry(p, n, uv)
        interaction.inst <- this
        interaction, pdf
    override this.EvalPDF(interaction: Interaction inref) =
        let e0 = Vector3.Transform(this.Primitive.E0, this.ObjectToWorld) - this.ObjectToWorld.Translation
        let e1 = Vector3.Transform(this.Primitive.E1, this.ObjectToWorld) - this.ObjectToWorld.Translation
        2f / Vector3.Cross(e0, e1).Length()


