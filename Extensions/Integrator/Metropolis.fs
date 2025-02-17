﻿#nowarn "9"
namespace Barnacle.Extensions.Integrator

open Barnacle.Base
open System
open System.Numerics
open System.Runtime.CompilerServices
open Microsoft.FSharp.NativeInterop

[<Struct>]
type PrimarySample =
    val mutable Value: float32
    val mutable ValueBackUp: float32
    val mutable LastModification: int
    val mutable ModificationBackUp: int

    member inline this.BackUp() =
        this.ValueBackUp <- this.Value
        this.ModificationBackUp <- this.LastModification

    member inline this.Restore() =
        this.Value <- this.ValueBackUp
        this.LastModification <- this.ModificationBackUp

[<Struct>]
type MLTSampler =
    val LargeStepProb: float32
    val Sigma: float32
    val mutable LargeStep: bool
    val mutable LastLargeStepIteration: int
    val mutable CurrentIteration: int
    val mutable SampleIndex: int
    val mutable InitializedSampleCount: int
    val mutable InnerSampler: Sampler
    val Xs: nativeptr<PrimarySample>

    new(innerSampler: Sampler, largeStepProb: float32, sigma: float32, xs: nativeptr<PrimarySample>) =
        {
            LargeStepProb = largeStepProb
            Sigma = sigma
            LargeStep = true
            LastLargeStepIteration = 0
            CurrentIteration = 0
            SampleIndex = 0
            InitializedSampleCount = 0
            InnerSampler = innerSampler
            Xs = xs
        }
    
    member inline this.RandomAccess(i: int) =
        let xs = NativePtr.toByRef this.Xs
        &Unsafe.Add(&xs, i)
    
    member inline this.StartIteration() =
        this.CurrentIteration <- this.CurrentIteration + 1
        this.SampleIndex <- 0
        this.LargeStep <- this.LargeStepProb > this.InnerSampler.Next1D()

    member inline this.GetNextIndex() =
        let index = this.SampleIndex
        this.SampleIndex <- index + 1
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

        if index >= this.InitializedSampleCount then
            this.InitializedSampleCount <- index + 1
        let x = &this.RandomAccess(index)

        if x.LastModification < this.LastLargeStepIteration then
            x.Value <- this.InnerSampler.Next1D()
            x.LastModification <- this.LastLargeStepIteration

        x.BackUp()

        x.Value <-
            if this.LargeStep then
                this.InnerSampler.Next1D()
            else
                let normalSample = MathF.Sqrt(2f) * ErfInv(MathF.FusedMultiplyAdd(2f, this.InnerSampler.Next1D(), -1f))

                let effectiveSigma =
                    this.Sigma * MathF.Sqrt(float32 (this.CurrentIteration - this.LastLargeStepIteration))

                let value = MathF.FusedMultiplyAdd(normalSample, effectiveSigma, x.Value)
                value - MathF.Floor(value)

        x.LastModification <- this.CurrentIteration
        x.Value

    member inline this.Next1D() =
        this.EnsureReady(this.GetNextIndex())

    member inline this.Next2D() = Vector2(this.Next1D(), this.Next1D())

    member inline this.Reject() =
        let xs = NativePtr.toByRef this.Xs
        for i = 0 to this.SampleIndex - 1 do
            Unsafe.Add(&xs, i).Restore()

        this.CurrentIteration <- this.CurrentIteration - 1

    member inline this.Accept() =
        if this.LargeStep then
            this.LastLargeStepIteration <- this.CurrentIteration

[<Sealed>]
type MetropolisIntegrator
    (nBootstrap: int, nChains: int, mutationPerPixel: int, sigma: float32, largeStepProb: float32) =
    inherit ProgressiveIntegrator(mutationPerPixel)
    new(mutationPerPixel: int) = MetropolisIntegrator(4096 * 1024, 256 * 1024, mutationPerPixel, 1e-4f, 0.5f)
    member this.MaxDepth = 8
    member this.RRDepth = 5
    member this.RRThreshold = 0.95f
    member this.NBootstrap = nBootstrap
    member this.NChains = nChains
    member this.Sigma = sigma
    member this.LargeStepProb = largeStepProb

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
        let tileXLast = min ((tx + 1) * this.TileSize) imageWidth
        let tileYLast = min ((ty + 1) * this.TileSize) imageHeight
        let tileWidth = tileXLast - tileXFirst
        let tileHeight = tileYLast - tileYFirst
        let tileSize = uint64 (tileWidth * tileHeight)
        let imageSize = uint64 (imageWidth * imageHeight)
        let nBootstrapPerTile = int ((uint64 this.NBootstrap * tileSize + imageSize - 1uL) / imageSize)
        let bootstrapWeights = Array.zeroCreate<float32> nBootstrapPerTile
        let xs = NativePtr.stackalloc<PrimarySample> (4 + 6 * this.MaxDepth)
        for bootstrapId = 0 to bootstrapWeights.Length - 1 do
            let innerSampler = Sampler(uint (bootstrapId + nBootstrapPerTile * this.FrameId), uint tx, uint ty)
            let mutable mltSampler = MLTSampler(innerSampler, this.LargeStepProb, this.Sigma, xs)
            let imageX = min (tileXLast - 1) (int (mltSampler.Next1D() * float32 tileWidth) + tileXFirst)
            let imageY = min (tileYLast - 1) (int (mltSampler.Next1D() * float32 tileHeight) + tileYFirst)
            let struct (ray, pdf) = camera.GeneratePrimaryRay(film.Resolution, struct (imageX, imageY), mltSampler.Next2D(), mltSampler.Next2D())
            let L = this.Li(&ray, aggregate, lightSampler, &mltSampler) * (1f / pdf)
            bootstrapWeights[bootstrapId] <- Single.Clamp(L.X, 0f, 1f) + Single.Clamp(L.Y, 0f, 1f) + Single.Clamp(L.Z, 0f, 1f)
        let invB = float32 nBootstrapPerTile / (Array.sum bootstrapWeights)
        let bootstrapSamplingTable = AliasTable(bootstrapWeights)
        let nChainsPerTile = int ((uint64 this.NChains * tileSize + imageSize - 1uL) / imageSize)
        let mutationsPerChain = int ((uint64 this.SamplePerPixel * tileSize + uint64 nChainsPerTile - 1UL) / uint64 nChainsPerTile)
        let effectiveSamplePerPixel = float32 (mutationsPerChain * nChainsPerTile) / float32 (tileWidth * tileHeight)
        for chainId = 0 to nChainsPerTile - 1 do
            let mutable sampler = Sampler(uint (13 * chainId + nChainsPerTile * this.FrameId), uint tx, uint ty)
            let bootstrapId, _ = bootstrapSamplingTable.Sample(sampler.Next1D())
            let innerSampler = Sampler(uint (bootstrapId + nBootstrapPerTile * this.FrameId), uint tx, uint ty)
            let mutable mltSampler = MLTSampler(innerSampler, this.LargeStepProb, this.Sigma, xs)
            let mutable uPixel = mltSampler.Next2D()
            uPixel.X <- uPixel.X * float32 tileWidth
            uPixel.Y <- uPixel.Y * float32 tileHeight
            let imageX = min (tileXLast - 1) (int uPixel.X + tileXFirst)
            let imageY = min (tileYLast - 1) (int uPixel.Y + tileYFirst)
            uPixel <- uPixel - Vector2(float32 imageX, float32 imageY)
            let mutable pixelId = struct (imageX, imageY)
            let struct (ray, pdf) = camera.GeneratePrimaryRay(film.Resolution, pixelId, uPixel, mltSampler.Next2D())
            let mutable L = this.Li(&ray, aggregate, lightSampler, &mltSampler) * (1f / pdf)
            let mutable y = Single.Clamp(L.X, 0f, 1f) + Single.Clamp(L.Y, 0f, 1f) + Single.Clamp(L.Z, 0f, 1f)
            let mutable radiance = Vector3.Zero
            for j = 0 to mutationsPerChain - 1 do
                mltSampler.StartIteration()
                let imageX = min (tileXLast - 1) (int (mltSampler.Next1D() * float32 tileWidth) + tileXFirst)
                let imageY = min (tileYLast - 1) (int (mltSampler.Next1D() * float32 tileHeight) + tileYFirst)
                let pixelIdNew = struct (imageX, imageY)
                let struct (ray, pdf) = camera.GeneratePrimaryRay(film.Resolution, pixelIdNew, mltSampler.Next2D(), mltSampler.Next2D())
                let LNew = this.Li(&ray, aggregate, lightSampler, &mltSampler) * (1f / pdf)
                let yNew = Single.Clamp(LNew.X, 0f, 1f) + Single.Clamp(LNew.Y, 0f, 1f) + Single.Clamp(LNew.Z, 0f, 1f)
                let accept = MathF.Min(1f, yNew / y)
                let wOld = (1f - accept) / Single.FusedMultiplyAdd(y, invB, this.LargeStepProb)
                radiance <- radiance + (wOld / effectiveSamplePerPixel) * L
                let wNew =
                    (accept + if mltSampler.LargeStep then 1f else 0f) / Single.FusedMultiplyAdd(yNew, invB, this.LargeStepProb)
                if sampler.Next1D() < accept then
                    film.Accumulate(pixelId, radiance)
                    radiance <- (wNew / effectiveSamplePerPixel) * LNew
                    pixelId <- pixelIdNew
                    L <- LNew
                    y <- yNew
                    mltSampler.Accept()
                else
                    film.Accumulate(pixelIdNew, (wNew / effectiveSamplePerPixel) * LNew)
                    mltSampler.Reject()
            film.Accumulate(pixelId, radiance)