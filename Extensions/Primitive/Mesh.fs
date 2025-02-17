﻿#nowarn "9"
namespace Barnacle.Extensions.Primitive


open Barnacle.Base
open System
open System.Numerics
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

[<IsReadOnly; Struct>]
type internal Triangle(p0: Vector3, p1: Vector3, p2: Vector3) =
    member this.P0 = p0
    member this.P1 = p1
    member this.P2 = p2
    
    member inline this.Bounds =
        let pMin = Vector3.Min(Vector3.Min(p0, p1), p2)
        let pMax = Vector3.Max(Vector3.Max(p0, p1), p2)
        AxisAlignedBoundingBox(pMin, pMax)

    member inline this.Intersect(ray: Ray inref, t: float32) =
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

    member inline this.Intersect(ray: Ray inref, geom: LocalGeometry outref, t: float32 byref) =
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

    member inline this.SurfaceArea =
        let e0 = this.P1 - this.P0
        let e1 = this.P2 - this.P0
        0.5f * Vector3.Cross(e0, e1).Length()

    member inline this.Sample(uSurface: Vector2) =
        let uv =
            if uSurface.X < uSurface.Y then
                Vector2(0.5f * uSurface.X, uSurface.Y - 0.5f * uSurface.X)
            else
                Vector2(uSurface.X - 0.5f * uSurface.Y, 0.5f * uSurface.Y)

        let p = uv.X * this.P1 + uv.Y * this.P2 + (1f - uv.X - uv.Y) * this.P0
        let n = Vector3.Cross(this.P1 - this.P0, this.P2 - this.P0)
        let pdf = 2f / n.Length()
        LocalGeometry(p, Vector3.Normalize(n), uv), pdf

    static member inline Transform(triangle: Triangle inref, m: Matrix4x4) =
        let p0 = Vector3.Transform(triangle.P0, m)
        let p1 = Vector3.Transform(triangle.P1, m)
        let p2 = Vector3.Transform(triangle.P2, m)
        Triangle(p0, p1, p2)

    static member inline Transform(triangle: Triangle, m: Matrix4x4) =
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
    let inline stackalloc<'a when 'a: unmanaged> (length: int) : 'a byref =
        let p = NativePtr.stackalloc<'a> length
        NativePtr.toByRef p
    
    let inline stackPush<'a when 'a: unmanaged> (stack: 'a byref, stackTop: int byref, value: 'a) =
        let stackTopElement = &Unsafe.Add(&stack, stackTop)
        stackTopElement <- value
        stackTop <- stackTop + 1
    
    let inline stackPop<'a when 'a: unmanaged> (stack: 'a byref, stackTop: int byref) : 'a =
        stackTop <- stackTop - 1
        Unsafe.Add(&stack, stackTop)

[<Sealed>]
type MeshPrimitive(vertices: Vector3 array, indices: int array) =
    inherit ElementalPrimitive()

    let triangleIndices =
        Array.init (indices.Length / 3) (fun i -> TriangleIndex(indices[i * 3], indices[i * 3 + 1], indices[i * 3 + 2]))

    member val private TriangleIndices = triangleIndices with get

    member val private BVHNodes =
        let builder = BVHBuilder.Default

        if builder.Config.MaxDepth > 128 then
            failwith "BVH depth potentially exceeds 128"

        builder.Build(
            triangleIndices.AsSpan(),
            (fun triangleIndex ->
                let p0 = vertices[triangleIndex.I0]
                let p1 = vertices[triangleIndex.I1]
                let p2 = vertices[triangleIndex.I2]
                let pMin = Vector3.Min(Vector3.Min(p0, p1), p2)
                let pMax = Vector3.Max(Vector3.Max(p0, p1), p2)
                AxisAlignedBoundingBox(pMin, pMax))
        ) with get

    member private this.Vertices = vertices
    member this.TriangleCount = this.TriangleIndices.Length

    member inline internal this.FetchTriangle(i: int) =
        let triangleIndices = &MemoryMarshal.GetArrayDataReference this.TriangleIndices
        let triangleIndex = Unsafe.Add(&triangleIndices, i)
        let vertices = &MemoryMarshal.GetArrayDataReference this.Vertices
        let p0 = &Unsafe.Add(&vertices, triangleIndex.I0)
        let p1 = &Unsafe.Add(&vertices, triangleIndex.I1)
        let p2 = &Unsafe.Add(&vertices, triangleIndex.I2)
        Triangle(p0, p1, p2)
    
    member val AliasTable =
        let weights = Array.init triangleIndices.Length (fun i ->
            let idx = triangleIndices[i]
            let triangle = Triangle(vertices[idx.I0], vertices[idx.I1], vertices[idx.I2])
            triangle.SurfaceArea)
        AliasTable(weights) with get

    override this.Intersect(ray: Ray inref, t: float32) =
        let traversalStack = &BLASTraversal.stackalloc<int> 128
        let mutable stackTop = 1
        BLASTraversal.stackPush(&traversalStack, &stackTop, 0)
        let nodes = &MemoryMarshal.GetArrayDataReference this.BVHNodes
        let mutable hit = false

        while not hit && stackTop <> 0 do
            let i = BLASTraversal.stackPop(&traversalStack, &stackTop)
            let node = Unsafe.Add(&nodes, i)

            if node.Bounds.Intersect(&ray, t) then
                if node.IsLeaf then
                    let mutable instanceId = node.InstanceOffset

                    while not hit && instanceId < node.InstanceOffset + node.InstanceCount do
                        let triangle = this.FetchTriangle(instanceId)
                        if triangle.Bounds.Intersect(&ray, t) then
                            hit <- triangle.Intersect(&ray, t)
                        instanceId <- instanceId + 1
                else
                    if ray.Direction[node.SplitAxis] > 0f then
                        BLASTraversal.stackPush(&traversalStack, &stackTop, node.RightChild)
                        BLASTraversal.stackPush(&traversalStack, &stackTop, i + 1)
                    else
                        BLASTraversal.stackPush(&traversalStack, &stackTop, i + 1)
                        BLASTraversal.stackPush(&traversalStack, &stackTop, node.RightChild)

        hit

    override this.Intersect(ray: Ray inref, geom: LocalGeometry outref, t: float32 byref) =
        let traversalStack = &BLASTraversal.stackalloc<int> 128
        let mutable stackTop = 0
        BLASTraversal.stackPush(&traversalStack, &stackTop, 0)
        let nodes = &MemoryMarshal.GetArrayDataReference this.BVHNodes
        let mutable hit = false

        while stackTop <> 0 do
            let i = BLASTraversal.stackPop(&traversalStack, &stackTop)
            let node = Unsafe.Add(&nodes, i)

            if node.Bounds.Intersect(&ray, t) then
                if node.IsLeaf then
                    for instanceId = node.InstanceOffset to node.InstanceOffset + node.InstanceCount - 1 do
                        let triangle = this.FetchTriangle(instanceId)
                        if triangle.Bounds.Intersect(&ray, t) then
                            hit <- triangle.Intersect(&ray, &geom, &t) || hit
                else
                    if ray.Direction[node.SplitAxis] > 0f then
                        BLASTraversal.stackPush(&traversalStack, &stackTop, node.RightChild)
                        BLASTraversal.stackPush(&traversalStack, &stackTop, i + 1)
                    else
                        BLASTraversal.stackPush(&traversalStack, &stackTop, i + 1)
                        BLASTraversal.stackPush(&traversalStack, &stackTop, node.RightChild)

        hit

    override this.Bounds = this.BVHNodes[0].Bounds

    static member inline Load(path: string) =
        let vertices = ResizeArray<Vector3>()
        let indices = ResizeArray<int>()
        use reader = new StreamReader(path)

        while not reader.EndOfStream do
            let line = reader.ReadLine().Trim()

            if line.StartsWith "v " then
                let tokens = line.Substring(2).Split(' ', StringSplitOptions.RemoveEmptyEntries)
                assert (tokens.Length = 3)
                vertices.Add(Vector3(float32 tokens[0], float32 tokens[1], float32 tokens[2]))
            elif line.StartsWith "f " then
                let tokens = line.Substring(2).Split(' ', StringSplitOptions.RemoveEmptyEntries)
                assert (tokens.Length >= 3)

                let retrieveIndex (token: string) =
                    match token.IndexOf('/') with
                    | -1 -> token
                    | idx -> token.Substring(0, idx)
                    |> int
                    |> fun i -> if i < 0 then vertices.Count + i else i - 1

                let i0 = retrieveIndex tokens[0]
                let mutable i1 = retrieveIndex tokens[1]

                for i = 2 to tokens.Length - 1 do
                    let i2 = retrieveIndex tokens[i]
                    indices.Add(i0)
                    indices.Add(i1)
                    indices.Add(i2)
                    i1 <- i2

        MeshPrimitive(vertices.ToArray(), indices.ToArray())

[<Sealed>]
type MeshInstance(mesh: MeshPrimitive, material: MaterialBase option, light: LightBase option) =
    inherit PrimitiveInstance(mesh, material, light)
    member this.Instance = mesh
    new(mesh: MeshPrimitive, material: MaterialBase, light: LightBase) = MeshInstance(mesh, Some material, Some light)
    new(mesh: MeshPrimitive, material: MaterialBase) = MeshInstance(mesh, Some material, None)
    new(mesh: MeshPrimitive, light: LightBase) = MeshInstance(mesh, None, Some light)

    override this.Sample(uSelect: float32, uSurface: Vector2) =
        let i, pdfTriangle = this.Instance.AliasTable.Sample(uSelect)
        let mutable triangle = this.Instance.FetchTriangle(i)
        triangle <- Triangle.Transform(&triangle, this.ObjectToWorld)
        let mutable interaction = Unchecked.defaultof<Interaction>
        let mutable geom, pdfArea = triangle.Sample(uSurface)
        geom.tag <- i
        interaction.geom <- geom
        interaction.inst <- this
        struct (interaction, pdfTriangle * pdfArea)

    override this.EvalPDF(interaction: Interaction inref) =
        let i = interaction.geom.tag
        let mutable triangle = this.Instance.FetchTriangle(i)
        triangle <- Triangle.Transform(&triangle, this.ObjectToWorld)
        1f / float32 this.Instance.TriangleCount * triangle.SurfaceArea
