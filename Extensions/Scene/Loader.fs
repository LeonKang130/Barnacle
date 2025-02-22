namespace Barnacle.Extensions.Scene

open Barnacle.Base
open Barnacle.Extensions.Material
open System
open System.Numerics
open System.IO
open System.Text.Json
open Barnacle.Extensions.Primitive

type KeyFrameInfo =
    {
        time: float32 Nullable
        scale: float32 array
        rotation: float32 array
        translation: float32 array
    }
    member this.ToKeyFrame() =
        let scale =
            if this.scale <> null then
                Matrix4x4.CreateScale(this.scale[0], this.scale[1], this.scale[2])
            else
                Matrix4x4.Identity
        let rotation =
            if this.rotation <> null then
                Matrix4x4.CreateRotationX(this.rotation[0]) * Matrix4x4.CreateRotationY(this.rotation[1]) * Matrix4x4.CreateRotationZ(this.rotation[2])
            else
                Matrix4x4.Identity
        let translation =
            if this.translation <> null then
                Matrix4x4.CreateTranslation(this.translation[0], this.translation[1], this.translation[2])
            else
                Matrix4x4.Identity
        KeyFrame(this.time.GetValueOrDefault(0f), scale * rotation * translation)

type TransformInfo =
    {
        keyframes: KeyFrameInfo array
    }
    member this.ToTransform() =
        let keyframes =
            this.keyframes
            |> Array.map _.ToKeyFrame()
            |> Array.sortBy (_.Time)
        Transform(keyframes)

type MaterialInfo =
    {
        ``type``: string
        albedo: float32 array
        ior: float32 Nullable
    }
    member this.ToMaterial(): MaterialBase =
        match this.``type`` with
        | "lambertian" ->
            let albedo =
                if this.albedo <> null then
                    Vector3(this.albedo[0], this.albedo[1], this.albedo[2])
                else
                    Vector3.One
            Lambertian(albedo)
        | "mirror" ->
            MirrorMaterial()
        | "dielectric" ->
            DielectricMaterial(this.ior.GetValueOrDefault(1.5f))
        | _ -> failwith $"Unknown material type: {this.``type``}"

type LightInfo =
    {
        ``type``: string
        emission: float32 array
        ``two-sided``: bool Nullable
    }
    member this.ToLight(): LightBase =
        match this.``type`` with
        | "diffuse" ->
            let emission =
                if this.emission <> null then
                    Vector3(this.emission[0], this.emission[1], this.emission[2])
                else
                    Vector3.Zero
            DiffuseLight(emission, this.``two-sided``.GetValueOrDefault(true))
        | _ -> failwith $"Unknown light type: {this.``type``}"

type PrimitiveInfo =
    {
        ``type``: string
        radius: float32 Nullable
        uri: string        
        vertices: float32 array
        indices: int array
    }
    member this.ToPrimitive(): ElementalPrimitive =
        match this.``type`` with
        | "sphere" ->
            SpherePrimitive(this.radius.GetValueOrDefault(1f))
        | "quad" -> MeshPrimitive.Quad
        | "cube" -> MeshPrimitive.Cube
        | "mesh" ->
            if this.uri <> null then
                MeshPrimitive.Load this.uri
            elif this.vertices <> null && this.indices <> null then
                let vertices =
                    this.vertices
                    |> Array.chunkBySize 3
                    |> Array.map (fun v -> Vector3(v[0], v[1], v[2]))
                MeshPrimitive(vertices, this.indices)
            else
                failwith "Invalid mesh primitive"
        | _ -> failwith $"Unknown primitive type: {this.``type``}"

type PrimitiveInstanceInfo =
    {
        primitive: int
        material: int Nullable
        light: int Nullable
    }
    member this.ToPrimitiveInstance(primitives: ElementalPrimitive array, materials: MaterialBase array, lights: LightBase array): PrimitiveInstance =
        let primitive = primitives[this.primitive]
        let material =
            if this.material.HasValue then
                Some materials[this.material.Value]
            else
                None
        let light =
            if this.light.HasValue then
                Some lights[this.light.Value]
            else
                None
        if primitive.GetType() = typeof<SpherePrimitive> then
            SphereInstance(primitive :?> SpherePrimitive, material, light)
        elif primitive.GetType() = typeof<MeshPrimitive> then
            MeshInstance(primitive :?> MeshPrimitive, material, light)
        else
            failwith $"Unknown primitive type: {primitive.GetType()}"

type NodeInfo =
    {
        instances: int array
        transform: int Nullable
        children: int array
    }

type SceneInfo =
    {
        root: int Nullable
        nodes: NodeInfo array
        instances: PrimitiveInstanceInfo array
        transforms: TransformInfo array
        primitives: PrimitiveInfo array
        materials: MaterialInfo array
        lights: LightInfo array
    }
    member this.ToScene() =
        let transforms =
            this.transforms
            |> Array.map _.ToTransform()
        let primitives =
            this.primitives
            |> Array.map _.ToPrimitive()
        let materials =
            this.materials
            |> Array.map _.ToMaterial()
        let lights =
            this.lights
            |> Array.map _.ToLight()
        let instances =
            this.instances
            |> Array.map _.ToPrimitiveInstance(primitives, materials, lights)
        let nodes =
            this.nodes
            |> Array.map (fun nodeInfo ->
                let transform =
                    if nodeInfo.transform.HasValue then
                        transforms[nodeInfo.transform.Value]
                    else
                        Transform()
                let instances =
                    if nodeInfo.instances <> null then
                        nodeInfo.instances
                        |> Array.map (fun i -> instances[i])
                    else
                        [||]
                Node(transform, instances))
        for i = 0 to nodes.Length - 1 do
            if this.nodes[i].children <> null then
                for j in this.nodes[i].children do
                    nodes[i].Children.Add(nodes[j])
        Scene(nodes[this.root.GetValueOrDefault(0)])

[<AutoOpen>]
module SceneExtensions =
    type Scene with
        static member Load(filename: string) =
            filename
            |> File.ReadAllText
            |> JsonSerializer.Deserialize<SceneInfo>
            |> _.ToScene()
