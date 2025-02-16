namespace Barnacle.Extensions.Integrator

open System.Runtime.CompilerServices
open Barnacle.Base
open System
open System.Numerics
open System.Threading.Tasks

[<Struct>]
type PrimarySample =
    { mutable value: float32
      mutable valueBackUp: float32
      mutable lastModification: uint
      mutable modificationBackUp: uint }

    member inline this.BackUp() =
        this.valueBackUp <- this.value
        this.modificationBackUp <- this.lastModification

    member inline this.Restore() =
        this.value <- this.valueBackUp
        this.lastModification <- this.modificationBackUp

[<Struct>]
type MLTSampler =
    val mutable Sampler: Sampler
    val LargeStepProb: float32
    val Sigma: float32
    val Xs: ResizeArray<PrimarySample>
    val mutable SampleIndex: int
    val mutable LastLargeStepIteration: uint
    val mutable LargeStep: bool
    val mutable CurrentIteration: uint

    new(seed: uint, largeStepProb: float32, sigma: float32) =
        { Sampler = Sampler(seed)
          LargeStepProb = largeStepProb
          Sigma = sigma
          Xs = ResizeArray()
          SampleIndex = 0
          LastLargeStepIteration = 0u
          LargeStep = true
          CurrentIteration = 0u }

    member inline this.StartIteration() =
        this.CurrentIteration <- this.CurrentIteration + 1u
        this.SampleIndex <- 0
        this.LargeStep <- this.Sampler.Next1D() < this.LargeStepProb

    member inline this.GetNextIndex() =
        let index = this.SampleIndex
        this.SampleIndex <- this.SampleIndex + 1
        index

    member inline this.EnsureReady(index: int) =
        let inline ErfInv (x: float32) =
            let mutable x = MathF.Min(MathF.Max(x, -0.99999f), 0.99999f)
            let mutable w = MathF.Log((1f - x) * (1f + x))

            if w < 5f then
                w <- w - 2.5f
                let mutable p = 2.81022636e-08f
                p <- MathF.FusedMultiplyAdd(p, w, 3.43273939e-07f)
                p <- MathF.FusedMultiplyAdd(p, w, -3.5233877e-06f)
                p <- MathF.FusedMultiplyAdd(p, w, -4.39150654e-06f)
                p <- MathF.FusedMultiplyAdd(p, w, 0.00021858087f)
                p <- MathF.FusedMultiplyAdd(p, w, -0.00125372503f)
                p <- MathF.FusedMultiplyAdd(p, w, -0.00417768164f)
                p <- MathF.FusedMultiplyAdd(p, w, 0.246640727f)
                MathF.FusedMultiplyAdd(p, w, 1.50140941f) * x
            else
                w <- MathF.Sqrt(w) - 3f
                let mutable p = -0.000200214257f
                p <- MathF.FusedMultiplyAdd(p, w, 0.000100950558f)
                p <- MathF.FusedMultiplyAdd(p, w, 0.00134934322f)
                p <- MathF.FusedMultiplyAdd(p, w, -0.00367342844f)
                p <- MathF.FusedMultiplyAdd(p, w, 0.00573950773f)
                p <- MathF.FusedMultiplyAdd(p, w, -0.0076224613f)
                p <- MathF.FusedMultiplyAdd(p, w, 0.00943887047f)
                p <- MathF.FusedMultiplyAdd(p, w, 1.00167406f)
                MathF.FusedMultiplyAdd(p, w, 2.83297682f) * x

        if index >= this.Xs.Count then
            this.Xs.Add Unchecked.defaultof<PrimarySample>

        let mutable x = this.Xs[index]

        if x.lastModification < this.LastLargeStepIteration then
            x.value <- this.Sampler.Next1D()
            x.lastModification <- this.LastLargeStepIteration

        x.BackUp()

        x.value <-
            if this.LargeStep then
                this.Sampler.Next1D()
            else
                let normalSample = MathF.Sqrt(2f) * ErfInv(2f * this.Sampler.Next1D() - 1f)

                let effectiveSigma =
                    this.Sigma
                    * MathF.Sqrt(float32 (this.CurrentIteration - this.LastLargeStepIteration))

                let value = x.value + normalSample * effectiveSigma
                value - MathF.Floor(value)

        x.lastModification <- this.CurrentIteration
        this.Xs[index] <- x

    member inline this.Next1D() =
        let index = this.GetNextIndex()
        this.EnsureReady(index)
        this.Xs[index].value

    member inline this.Next2D() = Vector2(this.Next1D(), this.Next1D())

    member inline this.Reject() =
        for i = 0 to this.SampleIndex - 1 do
            this.Xs[i].Restore()

        this.CurrentIteration <- this.CurrentIteration - 1u

    member inline this.Accept() =
        if this.LargeStep then
            this.LastLargeStepIteration <- this.CurrentIteration

[<IsReadOnly; Struct>]
type MLTPathSample =
    { imageX: int
      imageY: int
      L: Vector3
      y: float32 }

    member this.PixelId = this.imageX, this.imageY

type MetropolisIntegrator
    (maxDepth: int, nBootstrap: int, nChains: int, mutationPerPixel: int, sigma: float32, largeStepProb: float32) =
    inherit ProgressiveIntegrator(mutationPerPixel)
    new(mutationPerPixel: int) = MetropolisIntegrator(8, 1024 * 1024, 256 * 1024, mutationPerPixel, 1e-4f, 0.3f)
    member this.MaxDepth = maxDepth
    member this.RRDepth = 5
    member this.RRThreshold = 0.95f
    member this.NBootstrap = nBootstrap
    member this.NChains = nChains
    member this.Sigma = sigma
    member this.LargeStepProb = largeStepProb

    member inline this.Li
        (
            camera: CameraBase,
            film: Film,
            aggregate: PrimitiveAggregate,
            lightSampler: LightSamplerBase,
            sampler: MLTSampler byref
        ) : MLTPathSample =
        let imageWidth, imageHeight = film.Resolution
        let imageX = sampler.Next1D() * float32 imageWidth |> int |> min (imageWidth - 1)
        let imageY = sampler.Next1D() * float32 imageHeight |> int |> min (imageHeight - 1)

        let mutable ray, pdf =
            camera.GeneratePrimaryRay(film.Resolution, (imageX, imageY), sampler.Next2D(), sampler.Next2D())

        let mutable L = Vector3.Zero
        let mutable beta = Vector3(1f / pdf)
        let mutable depth = 0
        let mutable interaction = Unchecked.defaultof<Interaction>
        let mutable t = Unchecked.defaultof<float32>
        let mutable lightSample = Unchecked.defaultof<LightSample>
        let mutable bsdfSample = Unchecked.defaultof<BSDFSample>
        bsdfSample.eval.pdf <- 1e5f

        while depth < this.MaxDepth do
            t <- infinityf

            if not (aggregate.Intersect(&ray, &interaction, &t)) then
                depth <- this.MaxDepth
            else
                if interaction.HasLight then
                    lightSample.eval <- lightSampler.Eval(ray.Origin, &interaction)
                    let misWeight = bsdfSample.eval.pdf / (bsdfSample.eval.pdf + lightSample.eval.pdf)
                    L <- Vector3.FusedMultiplyAdd(beta, lightSample.eval.L * misWeight, L)

                if interaction.HasMaterial then
                    lightSample <- lightSampler.Sample(interaction.Position, sampler.Next1D(), sampler.Next2D())
                    let shadowRay = interaction.SpawnRay(lightSample.wi)
                    let lightDistance = (lightSample.eval.p - interaction.Position).Length()
                    let occluded = aggregate.Intersect(&shadowRay, lightDistance - 1e-3f)

                    if not occluded then
                        bsdfSample.eval <- interaction.EvalBSDF(-ray.Direction, lightSample.wi)

                        L <- Vector3.FusedMultiplyAdd(beta * bsdfSample.eval.bsdf,
                            lightSample.eval.L * (1f / (bsdfSample.eval.pdf + lightSample.eval.pdf)), L)

                    bsdfSample <- interaction.SampleBSDF(-ray.Direction, sampler.Next2D())
                    ray <- interaction.SpawnRay(bsdfSample.wi)
                    beta <- beta * bsdfSample.eval.bsdf / bsdfSample.eval.pdf

                    if depth >= this.RRDepth then
                        if sampler.Next1D() < this.RRThreshold then
                            beta <- beta / this.RRThreshold
                        else
                            depth <- this.MaxDepth

                    depth <- depth + 1
                else
                    depth <- this.MaxDepth

        if Single.IsNaN L.X then L.X <- 0f
        if Single.IsNaN L.Y then L.Y <- 0f
        if Single.IsNaN L.Z then L.Z <- 0f
        
        { imageX = imageX
          imageY = imageY
          L = L
          y = Vector3.Dot(Vector3(0.21f, 0.71f, 0.07f), Vector3.Clamp(L, Vector3.Zero, Vector3.One)) }

    override this.Render
        (camera: CameraBase, film: Film, aggregate: PrimitiveAggregate, lightSampler: LightSamplerBase)
        =
        let imageWidth, imageHeight = film.Resolution
        let bootstrapWeights = Array.zeroCreate<float32> this.NBootstrap

        let inline bootstrapSingleChain (bootstrapId: int) =
            let mutable sampler = MLTSampler(uint bootstrapId, this.LargeStepProb, this.Sigma)
            bootstrapWeights[bootstrapId] <- this.Li(camera, film, aggregate, lightSampler, &sampler).y

        Parallel.For(0, this.NBootstrap, bootstrapSingleChain) |> ignore
        let b = (Array.sum bootstrapWeights) / float32 this.NBootstrap
        let bootstrapSamplingTable = AliasTable(bootstrapWeights)

        let mutationsPerChain =
            (this.SamplePerPixel * (imageWidth * imageHeight) + this.NChains - 1)
            / this.NChains

        let effectiveSPP =
            float32 (mutationsPerChain * this.NChains) / float32 (imageWidth * imageHeight)

        let inline renderChain (chainId: int) =
            let mutable sampler = Sampler(uint chainId, 0x12345678u)
            let bootstrapIndex, _ = bootstrapSamplingTable.Sample(sampler.Next1D())

            let mutable mltSampler =
                MLTSampler(uint bootstrapIndex, this.LargeStepProb, this.Sigma)

            let mutable s = this.Li(camera, film, aggregate, lightSampler, &mltSampler)
            let mutable sNew = Unchecked.defaultof<MLTPathSample>

            for j = 0 to mutationsPerChain - 1 do
                mltSampler.StartIteration()
                sNew <- this.Li(camera, film, aggregate, lightSampler, &mltSampler)
                let accept = MathF.Min(1f, sNew.y / s.y)
                let wOld = (1f - accept) / (s.y / b + this.LargeStepProb)
                film.Accumulate(s.PixelId, (wOld / effectiveSPP) * s.L)

                let wNew =
                    (accept + if mltSampler.LargeStep then 1f else 0f)
                    / (sNew.y / b + this.LargeStepProb)

                film.Accumulate(sNew.PixelId, (wNew / effectiveSPP) * sNew.L)

                if sampler.Next1D() < accept then
                    s <- sNew
                    mltSampler.Accept()
                else
                    mltSampler.Reject()

        Parallel.For(0, this.NChains, renderChain) |> ignore
