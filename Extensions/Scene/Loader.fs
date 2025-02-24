namespace Barnacle.Extensions.Scene

open Barnacle.Base
open Barnacle.Extensions.Camera
open Barnacle.Extensions.Integrator
open Barnacle.Extensions.Material
open Barnacle.Extensions.Primitive
open System
open System.Numerics
open System.IO
open System.Text.Json


type KeyFrameInfo =
    { time: float32 Nullable
      scale: float32 array
      rotation: float32 array
      translation: float32 array }

    member inline this.ToKeyFrame() =
        let scale =
            if this.scale <> null then
                Matrix4x4.CreateScale(this.scale[0], this.scale[1], this.scale[2])
            else
                Matrix4x4.Identity

        let rotation =
            if this.rotation <> null then
                Matrix4x4.CreateRotationX(this.rotation[0])
                * Matrix4x4.CreateRotationY(this.rotation[1])
                * Matrix4x4.CreateRotationZ(this.rotation[2])
            else
                Matrix4x4.Identity

        let translation =
            if this.translation <> null then
                Matrix4x4.CreateTranslation(this.translation[0], this.translation[1], this.translation[2])
            else
                Matrix4x4.Identity

        KeyFrame(this.time.GetValueOrDefault(0f), scale * rotation * translation)

type TransformInfo =
    { keyframes: KeyFrameInfo array }

    member inline this.ToTransform() =
        let keyframes = this.keyframes |> Array.map _.ToKeyFrame() |> Array.sortBy (_.Time)
        Transform(keyframes)

type MaterialInfo =
    { ``type``: string
      albedo: float32 array
      ior: float32 Nullable
      roughness: float32 Nullable
      metallic: float32 Nullable }

    member inline this.ToMaterial() : MaterialBase =
        let baseColor =
            if this.albedo <> null then
                Vector3(this.albedo[0], this.albedo[1], this.albedo[2])
            else
                Vector3.One

        let ior = this.ior.GetValueOrDefault(1.5f)
        let roughness = this.roughness.GetValueOrDefault(1f)
        let metallic = this.metallic.GetValueOrDefault(0f)

        match this.``type`` with
        | "lambertian" -> Lambertian(baseColor)
        | "mirror" -> MirrorMaterial(baseColor)
        | "dielectric" -> DielectricMaterial(baseColor, ior)
        | "pbr" -> PBRMaterial(baseColor, metallic, roughness)
        | _ -> failwith $"Unknown material type: {this.``type``}"

type LightInfo =
    { ``type``: string
      emission: float32 array
      ``two-sided``: bool Nullable }

    member inline this.ToLight() : LightBase =
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
    { ``type``: string
      radius: float32 Nullable
      uri: string
      vertices: float32 array
      indices: int array }

    member inline this.ToPrimitive() : ElementalPrimitive =
        match this.``type`` with
        | "sphere" -> SpherePrimitive(this.radius.GetValueOrDefault(1f))
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
    { primitive: int
      material: int Nullable
      light: int Nullable }

    member inline this.ToPrimitiveInstance
        (primitives: ElementalPrimitive array, materials: MaterialBase array, lights: LightBase array)
        : PrimitiveInstance =
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
    { instances: int array
      transform: int Nullable
      children: int array
      ``has-camera``: bool Nullable }

    member inline this.ToNode(transforms: Transform array, instances: PrimitiveInstance array) =
        let transform =
            if this.transform.HasValue then
                transforms[this.transform.Value]
            else
                Transform()

        let hasCamera = this.``has-camera``.GetValueOrDefault(false)

        let instances' =
            if this.instances = null then
                [||]
            else
                this.instances |> Array.map (fun idx -> instances[idx])

        Node(transform, instances', hasCamera)

type IntegratorInfo =
    { ``type``: string
      ``max-depth``: int Nullable
      ``rr-depth``: int Nullable
      ``n-bootstrap``: int Nullable
      ``n-chains``: int Nullable
      ``mutation-strategy``: string
      ``large-step-prob``: float32 Nullable
      spp: int Nullable }

    member inline this.ToIntegrator() : IntegratorBase =
        let spp = this.spp.GetValueOrDefault(1)
        let maxDepth = this.``max-depth``.GetValueOrDefault(8)
        let rrDepth = this.``rr-depth``.GetValueOrDefault(5)

        match this.``type`` with
        | "normal" -> NormalIntegrator(spp)
        | "direct" -> DirectIntegrator(spp)
        | "path-tracing" -> PathTracingIntegrator(spp, maxDepth, rrDepth)
        | "pssmlt" ->
            let nBootstrap = this.``n-bootstrap``.GetValueOrDefault(4 * 1024 * 1024)
            let nChains = this.``n-chains``.GetValueOrDefault(1024)

            let mutationStrategy =
                if this.``mutation-strategy`` = null then
                    MutationStrategy.Gaussian(1e-2f)
                else
                    match this.``mutation-strategy`` with
                    | "Gaussian" -> MutationStrategy.Gaussian(1e-2f)
                    | "Kelemen" -> MutationStrategy.Kelemen(1f / 1024f, 1f / 16f)
                    | _ -> failwith $"Unknown mutation strategy: {this.``mutation-strategy``}"

            let largeStepProb = this.``large-step-prob``.GetValueOrDefault(0.5f)
            PSSMLTIntegrator(maxDepth, rrDepth, nBootstrap, nChains, spp, mutationStrategy, largeStepProb)
        | _ -> failwith $"Unknown integrator type: {this.``type``}"

type CameraInfo =
    { ``type``: string
      fov: float32 Nullable
      ``aspect-ratio``: float32 Nullable
      aperture: float32 Nullable
      ``focus-distance``: float32 Nullable
      ``push-forward``: float32 Nullable }

    member inline this.ToCamera() : CameraBase =
        let fov = this.fov.GetValueOrDefault(45f)
        let aspectRatio = this.``aspect-ratio``.GetValueOrDefault(1f)
        let aperture = this.aperture.GetValueOrDefault(0f)
        let focusDistance = this.``focus-distance``.GetValueOrDefault(1f)
        let pushForward = this.``push-forward``.GetValueOrDefault(0f)

        match this.``type`` with
        | "pinhole" -> PinholeCamera(fov, aspectRatio, pushForward)
        | "thin-lens" -> ThinLensCamera(aperture, focusDistance, fov, aspectRatio, pushForward)
        | _ -> failwith $"Unknown camera type: {this.``type``}"

type FilmInfo =
    { width: int
      height: int
      ``tone-mapping``: string }

    member inline this.ToFilm() =
        let toneMapping =
            match this.``tone-mapping`` with
            | "aces" -> ToneMapping.Aces
            | "gamma" -> ToneMapping.Gamma
            | "identity" -> ToneMapping.Identity
            | _ -> failwith $"Unknown tone mapping type: {this.``tone-mapping``}"

        Film(struct (this.width, this.height), toneMapping)

type SceneInfo =
    { root: int Nullable
      nodes: NodeInfo array
      instances: PrimitiveInstanceInfo array
      transforms: TransformInfo array
      primitives: PrimitiveInfo array
      materials: MaterialInfo array
      lights: LightInfo array
      integrator: IntegratorInfo
      camera: CameraInfo
      film: FilmInfo }

    member inline this.ToScene() =
        let transforms = this.transforms |> Array.map _.ToTransform()
        let primitives = this.primitives |> Array.map _.ToPrimitive()
        let materials = this.materials |> Array.map _.ToMaterial()
        let lights = this.lights |> Array.map _.ToLight()

        let instances =
            this.instances |> Array.map _.ToPrimitiveInstance(primitives, materials, lights)

        let nodes = this.nodes |> Array.map _.ToNode(transforms, instances)
        let integrator = this.integrator.ToIntegrator()
        let camera = this.camera.ToCamera()
        let film = this.film.ToFilm()

        for i = 0 to nodes.Length - 1 do
            if this.nodes[i].children <> null then
                for j in this.nodes[i].children do
                    nodes[i].Children.Add(nodes[j])

        Scene(nodes[this.root.GetValueOrDefault(0)], integrator, camera, film)

[<AutoOpen>]
module SceneIOExtensions =
    type Scene with
        static member Load(filename: string) =
            filename
            |> File.ReadAllText
            |> JsonSerializer.Deserialize<SceneInfo>
            |> _.ToScene()
