namespace Barnacle.Extensions.Integrator

open Barnacle.Base
open System
open System.Numerics

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
    val mutable Xs: ResizeArray<PrimarySample>
    val mutable SampleIndex: int
    val mutable LastLargeStepIteration: uint
    val mutable LargeStep: bool
    val mutable CurrentIteration: uint

    new(sampler: Sampler, largeStepProb: float32, sigma: float32) =
        { Sampler = sampler
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
            let mutable w = MathF.Log(MathF.FusedMultiplyAdd(-x, x, 1f))

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
                let normalSample = MathF.Sqrt(2f) * ErfInv(MathF.FusedMultiplyAdd(2f, this.Sampler.Next1D(), -1f))

                let effectiveSigma =
                    this.Sigma
                    * MathF.Sqrt(float32 (this.CurrentIteration - this.LastLargeStepIteration))

                let value = MathF.FusedMultiplyAdd(normalSample, effectiveSigma, x.value)
                value - MathF.Floor(value)

        x.lastModification <- this.CurrentIteration
        this.Xs[index] <- x
        x.value

    member inline this.Next1D() =
        this.EnsureReady(this.GetNextIndex())

    member inline this.Next2D() = Vector2(this.Next1D(), this.Next1D())

    member inline this.Reject() =
        for i = 0 to this.SampleIndex - 1 do
            this.Xs[i].Restore()

        this.CurrentIteration <- this.CurrentIteration - 1u

    member inline this.Accept() =
        if this.LargeStep then
            this.LastLargeStepIteration <- this.CurrentIteration

[<Sealed>]
type MetropolisIntegrator
    (maxDepth: int, nBootstrap: int, nChains: int, mutationPerPixel: int, sigma: float32, largeStepProb: float32) =
    inherit ProgressiveIntegrator(mutationPerPixel)
    new(mutationPerPixel: int) = MetropolisIntegrator(8, 4096 * 1024, 256 * 1024, mutationPerPixel, 1e-4f, 0.3f)
    member this.MaxDepth = maxDepth
    member this.RRDepth = 5
    member this.RRThreshold = 0.95f
    member this.NBootstrap = nBootstrap
    member this.NChains = nChains
    member this.Sigma = sigma
    member this.LargeStepProb = largeStepProb
    member val BootstrapWeights = ResizeArray<float32>()

    member inline this.Li
        (ray: Ray inref, aggregate: PrimitiveAggregate, lightSampler: LightSamplerBase, sampler: MLTSampler byref) =
        let mutable ray = ray
        let mutable L = Vector3.Zero
        let mutable beta = Vector3.One
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

                    if lightSample.eval.pdf <> 0f && not (aggregate.Intersect(&shadowRay, lightDistance - 1e-3f)) then
                        bsdfSample.eval <- interaction.EvalBSDF(-ray.Direction, lightSample.wi)

                        L <- Vector3.FusedMultiplyAdd(beta * bsdfSample.eval.bsdf,
                            lightSample.eval.L * (1f / (bsdfSample.eval.pdf + lightSample.eval.pdf)), L)

                    bsdfSample <- interaction.SampleBSDF(-ray.Direction, sampler.Next2D())
                    if bsdfSample.eval.pdf = 0f then
                        depth <- this.MaxDepth
                    else
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
        
        L
        
    override this.RenderTile(tx: int, ty: int, camera: CameraBase, film: Film, aggregate: PrimitiveAggregate, lightSampler: LightSamplerBase) =
        let struct (imageWidth, imageHeight) = film.Resolution
        let tileXFirst = tx * this.TileSize
        let tileYFirst = ty * this.TileSize
        let tileXLast = min ((tx + 1) * this.TileSize) film.ImageWidth
        let tileYLast = min ((ty + 1) * this.TileSize) film.ImageHeight
        let tileWidth = tileXLast - tileXFirst
        let tileHeight = tileYLast - tileYFirst
        let tileSize = uint64 (tileWidth * tileHeight)
        let imageSize = uint64 (imageWidth * imageHeight)
        let nBootstrapPerTile = int ((uint64 this.NBootstrap * tileSize + imageSize - 1uL) / imageSize)
        let nChainsPerTile = int ((uint64 this.NChains * tileSize + imageSize - 1uL) / imageSize)
        let mutationsPerChain = int ((uint64 this.SamplePerPixel * tileSize + uint64 nChainsPerTile - 1UL) / uint64 nChainsPerTile)
        let effectiveSamplePerPixel = float32 (mutationsPerChain * nChainsPerTile) / float32 (tileWidth * tileHeight)
        let bootstrapWeights = Array.zeroCreate<float32> nBootstrapPerTile
        let mutable mltSampler = Unchecked.defaultof<MLTSampler>
        for bootstrapId = 0 to nBootstrapPerTile - 1 do
            mltSampler <- MLTSampler(Sampler(uint (bootstrapId + nBootstrapPerTile * this.FrameId), uint tx, uint ty), this.LargeStepProb, this.Sigma)
            let imageX = min (tileXLast - 1) (int (mltSampler.Next1D() * float32 tileWidth) + tileXFirst)
            let imageY = min (tileYLast - 1) (int (mltSampler.Next1D() * float32 tileHeight) + tileYFirst)
            let struct (ray, pdf) = camera.GeneratePrimaryRay(film.Resolution, (imageX, imageY), mltSampler.Next2D(), mltSampler.Next2D())
            let L = Vector3.Clamp(this.Li(&ray, aggregate, lightSampler, &mltSampler) * (1f / pdf), Vector3.Zero, Vector3.One)
            bootstrapWeights[bootstrapId] <- L.X + L.Y + L.Z
        let invB = float32 nBootstrapPerTile / (Array.sum bootstrapWeights)
        let bootstrapSamplingTable = AliasTable(bootstrapWeights)
        for chainId = 0 to nChainsPerTile - 1 do
            let mutable sampler = Sampler(uint (chainId + nChainsPerTile * this.FrameId), uint tx, uint ty)
            let bootstrapId, _ = bootstrapSamplingTable.Sample(sampler.Next1D())
            mltSampler <- MLTSampler(Sampler(uint (bootstrapId + nBootstrapPerTile * this.FrameId), uint tx, uint ty), this.LargeStepProb, this.Sigma)
            let mutable imageX = min (tileXLast - 1) (int (mltSampler.Next1D() * float32 tileWidth) + tileXFirst)
            let mutable imageY = min (tileYLast - 1) (int (mltSampler.Next1D() * float32 tileHeight) + tileYFirst)
            let struct (ray, pdf) = camera.GeneratePrimaryRay(film.Resolution, (imageX, imageY), mltSampler.Next2D(), mltSampler.Next2D())
            let mutable L = this.Li(&ray, aggregate, lightSampler, &mltSampler) * (1f / pdf)
            let mutable y = Vector3.Dot(Vector3.One, Vector3.Clamp(L, Vector3.Zero, Vector3.One))
            for j = 0 to mutationsPerChain - 1 do
                mltSampler.StartIteration()
                let imageXNew = min (tileXLast - 1) (int (mltSampler.Next1D() * float32 tileWidth) + tileXFirst)
                let imageYNew = min (tileYLast - 1) (int (mltSampler.Next1D() * float32 tileHeight) + tileYFirst)
                let struct (ray, pdf) = camera.GeneratePrimaryRay(film.Resolution, (imageXNew, imageYNew), mltSampler.Next2D(), mltSampler.Next2D())
                let LNew = this.Li(&ray, aggregate, lightSampler, &mltSampler) * (1f / pdf)
                let yNew = Vector3.Dot(Vector3.One, Vector3.Clamp(LNew, Vector3.Zero, Vector3.One))
                let accept = MathF.Min(1f, yNew / y)
                let wOld = (1f - accept) / Single.FusedMultiplyAdd(y, invB, this.LargeStepProb)
                film.Accumulate((imageX, imageY), (wOld / effectiveSamplePerPixel) * L)
                let wNew =
                    if mltSampler.LargeStep then
                        (accept + 1f) / Single.FusedMultiplyAdd(yNew, invB, this.LargeStepProb)
                    else
                        accept / Single.FusedMultiplyAdd(yNew, invB, this.LargeStepProb)
                film.Accumulate((imageXNew, imageYNew), (wNew / effectiveSamplePerPixel) * LNew)
                if sampler.Next1D() < accept then
                    imageX <- imageXNew
                    imageY <- imageYNew
                    L <- LNew
                    y <- yNew
                    mltSampler.Accept()
                else
                    mltSampler.Reject()