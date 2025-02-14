namespace Barnacle.Extensions.Primitive

open Barnacle.Base
open System.Numerics

type MeshPrimitive(vertices: Vertex array, indices: int array) =
    inherit ElementalPrimitive()
    let triangleIndices, bvhNodes =
        (fun i ->
            let v0 = vertices[indices[3 * i]]
            let v1 = vertices[indices[3 * i + 1]]
            let v2 = vertices[indices[3 * i + 2]]
            let pMin = Vector3.Min(Vector3.Min(v0.p, v1.p), v2.p)
            let pMax = Vector3.Max(Vector3.Max(v0.p, v1.p), v2.p)
            AxisAlignedBoundingBox(pMin, pMax)
        )
        |> Array.init (indices.Length / 3)
        |> BVHBuilder.Default.Build
    let indices = Array.init indices.Length (fun i -> indices[3 * triangleIndices[i / 3] + i % 3])
    member this.TriangleCount = indices.Length / 3
    member this.FetchTriangle(i: int) =
        let v0 = vertices[indices[3 * i]]
        let v1 = vertices[indices[3 * i + 1]]
        let v2 = vertices[indices[3 * i + 2]]
        TrianglePrimitive(v0, v1, v2)
    member private this.TraverseAny(ray: Ray inref, t: float32, i: int) =
        let node = bvhNodes[i]
        if node.Bounds.Intersect(&ray, t) then
            if node.IsLeaf then
                let mutable hit = false
                let mutable instanceId = node.InstanceOffset
                while not hit && instanceId < node.InstanceOffset + node.InstanceCount do
                    hit <- this.FetchTriangle(instanceId).Intersect(&ray, t)
                    instanceId <- instanceId + 1
                hit
            else
                if ray.Direction[node.SplitAxis] > 0f then
                    this.TraverseAny(&ray, t, i + 1) || this.TraverseAny(&ray, t, node.RightChild)
                else
                    this.TraverseAny(&ray, t, node.RightChild) || this.TraverseAny(&ray, t, i + 1)
        else
            false
    member private this.TraverseClosest(ray: Ray inref, geom: LocalGeometry outref, t: float32 byref, i: int) =
        let node = bvhNodes[i]
        if node.Bounds.Intersect(&ray, t) then
            if node.IsLeaf then
                let mutable hit = false
                for instanceId = node.InstanceOffset to node.InstanceOffset + node.InstanceCount - 1 do
                    hit <- this.FetchTriangle(instanceId).Intersect(&ray, &geom, &t) || hit
                hit
            else
                if ray.Direction[node.SplitAxis] > 0f then
                    let hit = this.TraverseClosest(&ray, &geom, &t, i + 1)
                    this.TraverseClosest(&ray, &geom, &t, node.RightChild) || hit
                else
                    let hit = this.TraverseClosest(&ray, &geom, &t, node.RightChild)
                    this.TraverseClosest(&ray, &geom, &t, i + 1) || hit
        else
            false
    override this.Intersect(ray: Ray inref, t: float32) =
        this.TraverseAny(&ray, t, 0)
    override this.Intersect(ray: Ray inref, geom: LocalGeometry outref, t: float32 byref) =
        this.TraverseClosest(&ray, &geom, &t, 0)
    override this.Bounds = bvhNodes[0].Bounds

type MeshInstance(mesh: MeshPrimitive, material: MaterialBase option, light: LightBase option) =
    inherit PrimitiveInstance(mesh, material, light)
    member this.Instance = mesh
    override this.Sample(uSurface: Vector2) =
        let mutable uSurface = uSurface
        uSurface.X <- float32 this.Instance.TriangleCount * uSurface.X
        let triangleId = min (this.Instance.TriangleCount - 1) (int uSurface.X)
        uSurface.X <- uSurface.X - float32 triangleId
        let uv =
            if uSurface.X < uSurface.Y then
                Vector2(0.5f * uSurface.X, -0.5f * uSurface.X + uSurface.Y)
            else
                Vector2(-0.5f * uSurface.Y + uSurface.X, 0.5f * uSurface.Y)
        let triangle = this.Instance.FetchTriangle(triangleId)
        let v0 = Vertex.Transform(triangle.V0, this.ObjectToWorld)
        let v1 = Vertex.Transform(triangle.V1, this.ObjectToWorld)
        let v2 = Vertex.Transform(triangle.V2, this.ObjectToWorld)
        let p = (1f - uv.X - uv.Y) * v0.p + uv.X * v1.p + uv.Y * v2.p
        let n = Vector3.Normalize((1f - uv.X - uv.Y) * v0.n + uv.X * v1.n + uv.Y * v2.n)
        let uv = (1f - uv.X - uv.Y) * v0.uv + uv.X * v1.uv + uv.Y * v2.uv
        let pdf = 2f / (float32 this.Instance.TriangleCount * Vector3.Cross(v1.p - v0.p, v2.p - v0.p).Length())
        let mutable interaction = Unchecked.defaultof<Interaction>
        interaction.geom <- LocalGeometry(p, n, uv)
        interaction.geom.tag <- triangleId
        interaction.inst <- this
        interaction, pdf
    override this.EvalPDF(interaction: Interaction inref) =
        let triangleId = interaction.geom.tag
        let triangle = this.Instance.FetchTriangle(triangleId)
        let e0 = Vector3.Transform(triangle.E0, this.ObjectToWorld) - this.ObjectToWorld.Translation
        let e1 = Vector3.Transform(triangle.E1, this.ObjectToWorld) - this.ObjectToWorld.Translation
        2f / (float32 this.Instance.TriangleCount * Vector3.Cross(e0, e1).Length())
        