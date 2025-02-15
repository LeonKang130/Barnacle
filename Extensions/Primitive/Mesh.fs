#nowarn "9"
namespace Barnacle.Extensions.Primitive

open System.Runtime.CompilerServices
open Barnacle.Base
open System
open System.Numerics
open Microsoft.FSharp.NativeInterop

[<IsReadOnly; Struct>]
type Triangle(p0: Vector3, p1: Vector3, p2: Vector3) =
    member this.P0 = p0
    member this.P1 = p1
    member this.P2 = p2
    member this.Intersect(ray: Ray inref, t: float32) =
        let eps = Single.Epsilon
        let e0 = this.P1 - this.P0
        let e1 = this.P2 - this.P0
        let rayCrossE1 = Vector3.Cross(ray.Direction, e1)
        let det = Vector3.Dot(e0, rayCrossE1)
        if MathF.Abs(det) < eps then
            false
        else
            let invDet = 1f / det
            let s = ray.Origin - this.P0
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
    member this.Intersect(ray: Ray inref, geom: LocalGeometry outref, t: float32 byref) =
        let eps = Single.Epsilon
        let e0 = this.P1 - this.P0
        let e1 = this.P2 - this.P0
        let rayCrossE1 = Vector3.Cross(ray.Direction, e1)
        let det = Vector3.Dot(e0, rayCrossE1)
        if MathF.Abs(det) < eps then
            false
        else
            let invDet = 1f / det
            let s = ray.Origin - this.P0
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
                        let mutable n = Vector3.Normalize(Vector3.Cross(e0, e1))
                        if Vector3.Dot(n, ray.Direction) > 0f then
                            n <- -n
                        let p = ray.PointAt t'
                        geom <- LocalGeometry(p, n, Vector2(u, v))
                        t <- t'
                        true
                    else
                        false
    member this.SurfaceArea =
        let e0 = this.P1 - this.P0
        let e1 = this.P2 - this.P0
        0.5f * Vector3.Cross(e0, e1).Length()
    member this.Sample(uSurface: Vector2) =
        let uv =
            if uSurface.X < uSurface.Y then
                Vector2(0.5f * uSurface.X, uSurface.Y - 0.5f * uSurface.X)
            else
                Vector2(uSurface.X - 0.5f * uSurface.Y, 0.5f * uSurface.Y)
        let p = uv.X * this.P1 + uv.Y * this.P2 + (1f - uv.X - uv.Y) * this.P0
        let n = Vector3.Cross(this.P1 - this.P0, this.P2 - this.P0)
        let pdf = 2f / n.Length()
        LocalGeometry(p, Vector3.Normalize(n), uv), pdf
    static member Transform(triangle: Triangle inref, m: Matrix4x4) =
        let p0 = Vector3.Transform(triangle.P0, m)
        let p1 = Vector3.Transform(triangle.P1, m)
        let p2 = Vector3.Transform(triangle.P2, m)
        Triangle(p0, p1, p2)
    static member Transform(triangle: Triangle, m: Matrix4x4) =
        let p0 = Vector3.Transform(triangle.P0, m)
        let p1 = Vector3.Transform(triangle.P1, m)
        let p2 = Vector3.Transform(triangle.P2, m)
        Triangle(p0, p1, p2)

[<IsReadOnly; Struct>]
type TriangleIndex(i0: int, i1: int, i2: int) =
    member this.I0 = i0
    member this.I1 = i1
    member this.I2 = i2

module private BLASTraversal =
    let inline stackalloc<'a when 'a: unmanaged> (length: int): Span<'a> =
        let p = NativePtr.stackalloc<'a> length |> NativePtr.toVoidPtr
        Span<'a>(p, length)

type MeshPrimitive(vertices: Vector3 array, indices: int array) =
    inherit ElementalPrimitive()
    let triangleIndices =
        Array.init (indices.Length / 3) (fun i ->
            TriangleIndex(indices[i * 3], indices[i * 3 + 1], indices[i * 3 + 2]))
    member val private TriangleIndices = triangleIndices with get
    member val private BVHNodes =
        let builder = BVHBuilder.Default
        if builder.Config.MaxDepth > 128 then
            failwith "BVH depth potentially exceeds 128"
        builder.Build(triangleIndices.AsSpan(), (fun triangleIndex ->
            let p0 = vertices[triangleIndex.I0]
            let p1 = vertices[triangleIndex.I1]
            let p2 = vertices[triangleIndex.I2]
            let pMin = Vector3.Min(Vector3.Min(p0, p1), p2)
            let pMax = Vector3.Max(Vector3.Max(p0, p1), p2)
            AxisAlignedBoundingBox(pMin, pMax)
        )) with get
    member private this.Vertices = vertices
    member this.TriangleCount = this.TriangleIndices.Length
    member this.FetchTriangle(i: int) =
        let triangleIndex = this.TriangleIndices[i]
        let p0 = this.Vertices[triangleIndex.I0]
        let p1 = this.Vertices[triangleIndex.I1]
        let p2 = this.Vertices[triangleIndex.I2]
        Triangle(p0, p1, p2)
    override this.Intersect(ray: Ray inref, t: float32) =
        let traversalStack = BLASTraversal.stackalloc<int> 128
        traversalStack[0] <- 0
        let mutable stackTop = 1
        let mutable hit = false
        while not hit && stackTop <> 0 do
            stackTop <- stackTop - 1
            let i = traversalStack[stackTop]
            let node = this.BVHNodes[i]
            if node.Bounds.Intersect(&ray, t) then
                if node.IsLeaf then
                    let mutable instanceId = node.InstanceOffset
                    while not hit && instanceId < node.InstanceOffset + node.InstanceCount do
                        let triangle = this.FetchTriangle(instanceId)
                        hit <- triangle.Intersect(&ray, t)
                        instanceId <- instanceId + 1
                else
                    if ray.Direction[node.SplitAxis] > 0f then
                        traversalStack[stackTop] <- node.RightChild
                        traversalStack[stackTop + 1] <- i + 1
                    else
                        traversalStack[stackTop] <- i + 1
                        traversalStack[stackTop + 1] <- node.RightChild
                    stackTop <- stackTop + 2
        hit
    override this.Intersect(ray: Ray inref, geom: LocalGeometry outref, t: float32 byref) =
        let traversalStack = BLASTraversal.stackalloc<int> 128
        traversalStack[0] <- 0
        let mutable stackTop = 1
        let mutable hit = false
        while stackTop <> 0 do
            stackTop <- stackTop - 1
            let i = traversalStack[stackTop]
            let node = this.BVHNodes[i]
            if node.Bounds.Intersect(&ray, t) then
                if node.IsLeaf then
                    for instanceId = node.InstanceOffset to node.InstanceOffset + node.InstanceCount - 1 do
                        let triangle = this.FetchTriangle(instanceId)
                        hit <- triangle.Intersect(&ray, &geom, &t) || hit
                else
                    if ray.Direction[node.SplitAxis] > 0f then
                        traversalStack[stackTop] <- node.RightChild
                        traversalStack[stackTop + 1] <- i + 1
                    else
                        traversalStack[stackTop] <- i + 1
                        traversalStack[stackTop + 1] <- node.RightChild
                    stackTop <- stackTop + 2
        hit
    override this.Bounds = this.BVHNodes[0].Bounds

type MeshInstance(mesh: MeshPrimitive, material: MaterialBase option, light: LightBase option) =
    inherit PrimitiveInstance(mesh, material, light)
    member this.Instance = mesh
    new(mesh: MeshPrimitive, material: MaterialBase, light: LightBase) =
        MeshInstance(mesh, Some material, Some light)
    new(mesh: MeshPrimitive, material: MaterialBase) =
        MeshInstance(mesh, Some material, None)
    new(mesh: MeshPrimitive, light: LightBase) =
        MeshInstance(mesh, None, Some light)
    override this.Sample(uSurface: Vector2) =
        let mutable uSurface = uSurface
        uSurface.X <- uSurface.X * float32 this.Instance.TriangleCount
        let i = min (this.Instance.TriangleCount - 1) (int uSurface.X)
        uSurface.X <- uSurface.X - float32 i
        let mutable triangle = this.Instance.FetchTriangle(i)
        triangle <- Triangle.Transform(&triangle, this.ObjectToWorld)
        let mutable interaction = Unchecked.defaultof<Interaction>
        let mutable geom, pdf = triangle.Sample(uSurface)
        geom.tag <- i
        interaction.geom <- geom
        interaction.inst <- this
        interaction, pdf / float32 this.Instance.TriangleCount
    override this.EvalPDF(interaction: Interaction inref) =
        let i = interaction.geom.tag
        let mutable triangle = this.Instance.FetchTriangle(i)
        triangle <- Triangle.Transform(&triangle, this.ObjectToWorld)
        1f / float32 this.Instance.TriangleCount * triangle.SurfaceArea
        