namespace Barnacle.Extensions.Primitive


open Barnacle.Util
open Barnacle.Base
open System
open System.Numerics
open System.IO
open System.Runtime.CompilerServices

[<Struct; NoEquality; NoComparison>]
type Triangle =
    val mutable P0: Vector3
    val mutable P1: Vector3
    val mutable P2: Vector3
    new(p0: Vector3, p1: Vector3, p2: Vector3) =
        { P0 = p0; P1 = p1; P2 = p2 }

    member inline this.Bounds =
        let pMin = Vector3.MinNative(Vector3.MinNative(this.P0, this.P1), this.P2)
        let pMax = Vector3.MaxNative(Vector3.MaxNative(this.P0, this.P1), this.P2)
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
                Vector2(0.5f * uSurface.X, Single.FusedMultiplyAdd(-0.5f, uSurface.X, uSurface.Y))
            else
                Vector2(Single.FusedMultiplyAdd(-0.5f, uSurface.Y, uSurface.X), 0.5f * uSurface.Y)

        let p = uv.X * this.P1 + uv.Y * this.P2 + (1f - uv.X - uv.Y) * this.P0
        let n = Vector3.Cross(this.P1 - this.P0, this.P2 - this.P0)
        let pdf = 2f / n.Length()
        LocalGeometry(p, (0.5f * pdf) * n, uv), pdf

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

[<Sealed>]
type MeshPrimitive(vertices: Vector3 array, indices: int array) =
    inherit ElementalPrimitive()

    let triangleIndices =
        Array.init (indices.Length / 3) (fun i -> TriangleIndex(indices[i * 3], indices[i * 3 + 1], indices[i * 3 + 2]))

    static member val Quad =
        let vertices = [|
            Vector3(-1f, 0f, -1f)
            Vector3(1f, 0f, -1f)
            Vector3(1f, 0f, 1f)
            Vector3(-1f, 0f, 1f)
        |]
        let indices = [| 0; 1; 2; 0; 2; 3 |]
        MeshPrimitive(vertices, indices) with get
    
    static member val Cube =
        let vertices = [|
            Vector3(-1f, -1f, -1f)
            Vector3(1f, -1f, -1f)
            Vector3(1f, 1f, -1f)
            Vector3(-1f, 1f, -1f)
            Vector3(-1f, -1f, 1f)
            Vector3(1f, -1f, 1f)
            Vector3(1f, 1f, 1f)
            Vector3(-1f, 1f, 1f)
        |]
        let indices = [|
            0; 2; 1; 0; 3; 2;
            4; 5; 6; 4; 6; 7;
            0; 1; 5; 0; 5; 4;
            3; 7; 6; 3; 6; 2;
            0; 4; 7; 0; 7; 3;
            1; 2; 6; 1; 6; 5
        |]
        MeshPrimitive(vertices, indices) with get
    
    member val TriangleIndices = triangleIndices with get

    member val BVHNodes =
        BVHNode.Build(
            triangleIndices.AsSpan(),
            (fun triangleIndex ->
                let p0 = vertices[triangleIndex.I0]
                let p1 = vertices[triangleIndex.I1]
                let p2 = vertices[triangleIndex.I2]
                let pMin = Vector3.MinNative(Vector3.MinNative(p0, p1), p2)
                let pMax = Vector3.MaxNative(Vector3.MaxNative(p0, p1), p2)
                AxisAlignedBoundingBox(pMin, pMax))
        ) with get

    member this.Vertices = vertices
    member this.TriangleCount = this.TriangleIndices.Length

    member inline this.Item(i: int) =
        let triangleIndex = this.TriangleIndices[i]
        let p0 = this.Vertices[triangleIndex.I0]
        let p1 = this.Vertices[triangleIndex.I1]
        let p2 = this.Vertices[triangleIndex.I2]
        Triangle(p0, p1, p2)
    
    member val AliasTable =
        let weights = Array.init triangleIndices.Length (fun i ->
            let idx = triangleIndices[i]
            let triangle = Triangle(vertices[idx.I0], vertices[idx.I1], vertices[idx.I2])
            triangle.SurfaceArea)
        AliasTable(weights) with get

    override this.Intersect(ray, t) =
        let traversalStack = Allocation.StackAlloc<int> (BVHBuildConfig.MaxDepth + 1)
        let mutable stackTop = 1
        Allocation.StackPush(traversalStack, &stackTop, 0)
        let mutable hit = false

        while not hit && stackTop <> 0 do
            let i = Allocation.StackPop(traversalStack, &stackTop)
            let node = this.BVHNodes[i]

            if node.Bounds.Intersect(&ray, t) then
                if node.IsLeaf then
                    let mutable instanceId = node.InstanceOffset

                    while not hit && instanceId < node.InstanceOffset + node.InstanceCount do
                        let triangle = this[instanceId]
                        if triangle.Bounds.Intersect(&ray, t) then
                            hit <- triangle.Intersect(&ray, t)
                        instanceId <- instanceId + 1
                else
                    if ray.Direction[node.SplitAxis] > 0f then
                        Allocation.StackPush(traversalStack, &stackTop, node.RightChild)
                        Allocation.StackPush(traversalStack, &stackTop, i + 1)
                    else
                        Allocation.StackPush(traversalStack, &stackTop, i + 1)
                        Allocation.StackPush(traversalStack, &stackTop, node.RightChild)

        hit

    override this.Intersect(ray, geom, t) =
        let traversalStack = Allocation.StackAlloc<int> 128
        let mutable stackTop = 0
        Allocation.StackPush(traversalStack, &stackTop, 0)
        let mutable hit = false

        while stackTop <> 0 do
            let i = Allocation.StackPop(traversalStack, &stackTop)
            let node = this.BVHNodes[i]

            if node.Bounds.Intersect(&ray, t) then
                if node.IsLeaf then
                    for instanceId = node.InstanceOffset to node.InstanceOffset + node.InstanceCount - 1 do
                        let triangle = this[instanceId]
                        if triangle.Bounds.Intersect(&ray, t) && triangle.Intersect(&ray, &geom, &t) then
                            geom.tag <- instanceId
                            hit <- true
                else
                    if ray.Direction[node.SplitAxis] > 0f then
                        Allocation.StackPush(traversalStack, &stackTop, node.RightChild)
                        Allocation.StackPush(traversalStack, &stackTop, i + 1)
                    else
                        Allocation.StackPush(traversalStack, &stackTop, i + 1)
                        Allocation.StackPush(traversalStack, &stackTop, node.RightChild)

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
    member this.Mesh = mesh
    new(mesh: MeshPrimitive, material: MaterialBase, light: LightBase) = MeshInstance(mesh, Some material, Some light)
    new(mesh: MeshPrimitive, material: MaterialBase) = MeshInstance(mesh, Some material, None)
    new(mesh: MeshPrimitive, light: LightBase) = MeshInstance(mesh, None, Some light)

    override this.Sample(uSelect, uSurface) =
        let i, pdfTriangle = this.Mesh.AliasTable.Sample(uSelect)
        let mutable triangle = this.Mesh[i]
        triangle <- Triangle.Transform(&triangle, this.ObjectToWorld)
        let mutable interaction = Unchecked.defaultof<Interaction>
        let mutable geom, pdfArea = triangle.Sample(uSurface)
        geom.tag <- i
        interaction.geom <- geom
        interaction.inst <- this
        struct (interaction, pdfTriangle * pdfArea)

    override this.EvalPDF(interaction) =
        let i = interaction.geom.tag
        let mutable triangle = this.Mesh[i]
        triangle <- Triangle.Transform(&triangle, this.ObjectToWorld)
        this.Mesh.AliasTable.Table[i].pdf / triangle.SurfaceArea
