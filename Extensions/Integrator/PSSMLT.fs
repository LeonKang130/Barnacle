#nowarn "9"
namespace Barnacle.Extensions.Integrator


open Barnacle.Base
open Barnacle.Util
open System
open System.Collections.Concurrent
open System.Numerics
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open Microsoft.FSharp.NativeInterop

[<IsReadOnly; Struct>]
type MutationStrategy =
    | Kelemen of epsMin: float32 * epsMax: float32
    | Gaussian of sigma: float32

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
    val mutable InnerSampler: Sampler
    val LargeStepProb: float32
    val Strategy: MutationStrategy
    val Xs: PrimarySample nativeptr
    val mutable LargeStep: bool
    val mutable LastLargeStepIteration: int
    val mutable CurrentIteration: int
    val mutable SampleIndex: int
    val mutable InitializedSampleCount: int

    new(innerSampler: Sampler, largeStepProb: float32, strategy: MutationStrategy, xs: nativeptr<PrimarySample>) =
        { InnerSampler = innerSampler
          LargeStepProb = largeStepProb
          Strategy = strategy
          Xs = xs
          LargeStep = false
          LastLargeStepIteration = 0
          CurrentIteration = 0
          SampleIndex = 0
          InitializedSampleCount = 0 }

    member inline this.StartIteration() =
        this.LargeStep <- this.CurrentIteration = 0 || this.InnerSampler.Next1D() < this.LargeStepProb
        this.CurrentIteration <- this.CurrentIteration + 1
        this.SampleIndex <- 0

    member inline this.GetNextIndex() =
        let index = this.SampleIndex
        this.SampleIndex <- index + 1
        index

    member inline this.EnsureReady(index: int) =
        let inline ErfInv (x: float32) =
            let mutable x = MathF.Min(MathF.Max(x, -0.99999f), 0.99999f)
            let mutable w = - MathF.Log(Single.FusedMultiplyAdd(x, -x, 1f))

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

        let x = &Allocation.StackRead(this.Xs, index)

        if this.InitializedSampleCount <= index then
            x <- Unchecked.defaultof<PrimarySample>
            this.InitializedSampleCount <- index + 1

        if x.LastModification < this.LastLargeStepIteration then
            x.Value <- this.InnerSampler.Next1D()
            x.LastModification <- this.LastLargeStepIteration

        x.BackUp()

        x.Value <-
            if this.LargeStep then
                this.InnerSampler.Next1D()
            else
                match this.Strategy with
                | Gaussian sigma ->
                    let normalSample =
                        MathF.Sqrt(2f)
                        * ErfInv(MathF.FusedMultiplyAdd(2f, this.InnerSampler.Next1D(), -1f))

                    let effectiveSigma =
                        sigma * MathF.Sqrt(float32 (this.CurrentIteration - x.LastModification))

                    MathF.FusedMultiplyAdd(normalSample, effectiveSigma, x.Value)
                | Kelemen(epsMin, epsMax) ->
                    let mutable value = x.Value
                    let a = MathF.Log(epsMax / epsMin)

                    for _ = x.LastModification to this.CurrentIteration - 1 do
                        let u1 = this.InnerSampler.Next1D() - 0.5f
                        let u2 = if u1 < 0f then 1f + 2f * u1 else 2f * u1

                        value <- value + MathF.CopySign(epsMax * MathF.Exp(-a * u2), u1)

                    value
                |> fun x -> x - MathF.Floor(x)

        x.LastModification <- this.CurrentIteration
        x.Value

    member inline this.Next1D() = this.EnsureReady(this.GetNextIndex())

    member inline this.Next2D() = Vector2(this.Next1D(), this.Next1D())

    member inline this.Reject() =
        for i = 0 to this.InitializedSampleCount - 1 do
            Allocation.StackRead(this.Xs, i).Restore()

        this.CurrentIteration <- this.CurrentIteration - 1

    member inline this.Accept() =
        if this.LargeStep then
            this.LastLargeStepIteration <- this.CurrentIteration

[<Sealed>]
type PSSMLTIntegrator
    (maxDepth: int, rrDepth: int, nBootstrap: int, nChains: int, mutationPerPixel: int, strategy: MutationStrategy, largeStepProb: float32) =
    inherit ProgressiveIntegrator(mutationPerPixel)

    member this.MaxDepth = maxDepth
    member this.RRDepth = rrDepth
    member this.NBootstrap = nBootstrap
    member this.NChains = nChains
    member this.Strategy = strategy
    member this.LargeStepProb = largeStepProb
    member val BootstrapWeights = Array.zeroCreate<float32> nBootstrap with get
    member val B = 0f with get, set

    [<DefaultValue>]
    val mutable AcceptedMutationCount: int

    [<DefaultValue>]
    val mutable ProposedMutationCount: int

    member inline this.Li
        (ray: Ray inref, aggregate: PrimitiveAggregate, lightSampler: LightSamplerBase, sampler: MLTSampler byref)
        =
        let mutable ray = ray
        let mutable L = Vector3.Zero
        let mutable beta = Vector3.One
        let mutable depth = 0
        let mutable interaction = Unchecked.defaultof<Interaction>
        let mutable t = Unchecked.defaultof<float32>
        let mutable lightSample = Unchecked.defaultof<LightSample>
        let mutable bsdfSample = Unchecked.defaultof<BSDFSample>

        while depth < this.MaxDepth do
            t <- infinityf

            if not (aggregate.Intersect(&ray, &interaction, &t)) then
                depth <- this.MaxDepth
            else
                if interaction.HasLight then
                    lightSample.eval <- lightSampler.Eval(ray.Origin, &interaction)

                    let misWeight =
                        if depth = 0 then
                            1f
                        else
                            bsdfSample.eval.pdf
                            * MathF.ReciprocalEstimate(lightSample.eval.pdf + bsdfSample.eval.pdf)

                    L <- Vector3.FusedMultiplyAdd(beta, lightSample.eval.L * misWeight, L)
                let uLight = sampler.Next1D()
                let uEmit = sampler.Next2D()
                let uLobe = sampler.Next1D()
                let uBSDF = sampler.Next2D()
                let uRussianRoulette = sampler.Next1D()
                if interaction.HasMaterial then
                    lightSample <- lightSampler.Sample(interaction.Position, uLight, uEmit)
                    let shadowRay = interaction.SpawnRay(lightSample.wi)
                    let lightDistance = (lightSample.eval.p - interaction.Position).Length()

                    if
                        lightSample.eval.pdf <> 0f
                        && not (aggregate.Intersect(&shadowRay, lightDistance - 1e-3f))
                    then
                        bsdfSample.eval <- interaction.EvalBSDF(-ray.Direction, lightSample.wi)

                        L <-
                            Vector3.FusedMultiplyAdd(
                                beta * bsdfSample.eval.bsdf,
                                lightSample.eval.L
                                * MathF.ReciprocalEstimate(bsdfSample.eval.pdf + lightSample.eval.pdf),
                                L
                            )

                    bsdfSample <- interaction.SampleBSDF(-ray.Direction, uLobe, uBSDF)

                    if bsdfSample.eval.pdf = 0f then
                        depth <- this.MaxDepth
                    else
                        ray <- interaction.SpawnRay(bsdfSample.wi)
                        beta <- beta * bsdfSample.eval.bsdf * MathF.ReciprocalEstimate(bsdfSample.eval.pdf)

                        if depth >= this.RRDepth then
                            let rrThreshold = MathF.Min(1f, MathF.Max(beta.X, MathF.Max(beta.Y, beta.Z)))

                            if uRussianRoulette < rrThreshold then
                                beta <- beta * MathF.ReciprocalEstimate(rrThreshold)
                            else
                                depth <- this.MaxDepth

                        depth <- depth + 1
                else
                    depth <- this.MaxDepth

        L

    member inline this.BootstrapSingleChain
        (bootstrapId: int, xs: nativeptr<PrimarySample>, camera: CameraBase, film: Film, aggregate: PrimitiveAggregate, lightSampler: LightSamplerBase) =
        let struct (imageWidth, imageHeight) = film.Resolution

        let mutable mltSampler =
            MLTSampler(Sampler(uint this.FrameId, uint bootstrapId), this.LargeStepProb, this.Strategy, xs)

        mltSampler.StartIteration()
        let uPixel = mltSampler.Next2D() * Vector2(float32 imageWidth, float32 imageHeight)
        let imageX = min (imageWidth - 1) (int uPixel.X)
        let imageY = min (imageHeight - 1) (int uPixel.Y)
        let pixelId = struct (imageX, imageY)

        let struct (ray, pdf) =
            camera.GeneratePrimaryRay(
                film.Resolution,
                pixelId,
                uPixel - Vector2(float32 imageX, float32 imageY),
                mltSampler.Next2D()
            )

        let L =
            this.Li(&ray, aggregate, lightSampler, &mltSampler)
            * MathF.ReciprocalEstimate(pdf)

        let y = Vector3.Dot(L, Vector3(0.2126f, 0.7152f, 0.0722f))
        this.BootstrapWeights[bootstrapId] <- y

    member inline this.RenderSingleChain
        (
            chainId: int,
            bootstrapSamplingTable: AliasTable,
            xs: nativeptr<PrimarySample>,
            camera: CameraBase,
            film: Film,
            aggregate: PrimitiveAggregate,
            lightSampler: LightSamplerBase
        ) =
        let struct (imageWidth, imageHeight) = film.Resolution
        let mutable sampler = Sampler(uint this.FrameId, uint chainId)
        let bootstrapId, _ = bootstrapSamplingTable.Sample(sampler.Next1D())

        let mutable mltSampler =
            MLTSampler(Sampler(uint this.FrameId, uint bootstrapId), this.LargeStepProb, this.Strategy, xs)

        mltSampler.StartIteration()
        let uPixel = mltSampler.Next2D() * Vector2(float32 imageWidth, float32 imageHeight)
        let imageX = min (imageWidth - 1) (int uPixel.X)
        let imageY = min (imageHeight - 1) (int uPixel.Y)
        let mutable pixelId = struct (imageX, imageY)

        let struct (ray, pdf) =
            camera.GeneratePrimaryRay(
                film.Resolution,
                pixelId,
                uPixel - Vector2(float32 imageX, float32 imageY),
                mltSampler.Next2D()
            )

        let mutable L =
            this.Li(&ray, aggregate, lightSampler, &mltSampler)
            * MathF.ReciprocalEstimate(pdf)

        let mutable y = Vector3.Dot(L, Vector3(0.2126f, 0.7152f, 0.0722f))
        mltSampler.Accept()
        mltSampler.InnerSampler <- Sampler(uint chainId, uint bootstrapId, uint this.FrameId)

        let mutationPerChain =
            int (
                (uint64 this.SamplePerPixel * uint64 imageWidth * uint64 imageHeight
                 + uint64 this.NChains
                 - 1uL)
                / uint64 this.NChains
            )

        let invEffectiveSamplePerPixel =
            MathF.ReciprocalEstimate(
                float32 mutationPerChain * float32 this.NChains
                / float32 (imageWidth * imageHeight)
            )

        let mutable radiance = Vector3.Zero
        let mutable acceptedMutationCount = 0
        let invB = MathF.ReciprocalEstimate(this.B)

        for _ = 0 to mutationPerChain - 1 do
            mltSampler.StartIteration()
            let uPixel = mltSampler.Next2D() * Vector2(float32 imageWidth, float32 imageHeight)
            let imageX = min (imageWidth - 1) (int uPixel.X)
            let imageY = min (imageHeight - 1) (int uPixel.Y)
            let mutable pixelIdNew = struct (imageX, imageY)

            let struct (ray, pdf) =
                camera.GeneratePrimaryRay(
                    film.Resolution,
                    pixelIdNew,
                    uPixel - Vector2(float32 imageX, float32 imageY),
                    mltSampler.Next2D()
                )

            let LNew =
                this.Li(&ray, aggregate, lightSampler, &mltSampler)
                * MathF.ReciprocalEstimate(pdf)

            let yNew = Vector3.Dot(LNew, Vector3(0.2126f, 0.7152f, 0.0722f))
            let accept = MathF.Min(1f, yNew / y)
            let wOld = (1f - accept) / MathF.FusedMultiplyAdd(y, invB, this.LargeStepProb)
            // let wOld = (1f - accept) / (y * invB)
            radiance <- radiance + wOld * L

            let wNew =
                (accept + if mltSampler.LargeStep then 1f else 0f)
                / MathF.FusedMultiplyAdd(yNew, invB, this.LargeStepProb)
            // let wNew = if yNew > 0f then accept / (yNew * invB) else 0f
            if sampler.Next1D() < accept then
                acceptedMutationCount <- acceptedMutationCount + 1
                film.Accumulate(pixelId, radiance * invEffectiveSamplePerPixel)
                radiance <- wNew * LNew
                pixelId <- pixelIdNew
                L <- LNew
                y <- yNew
                mltSampler.Accept()
            else
                if accept > 0f then
                    film.Accumulate(pixelIdNew, (wNew * invEffectiveSamplePerPixel) * LNew)

                mltSampler.Reject()

        film.Accumulate(pixelId, radiance * invEffectiveSamplePerPixel)
        Interlocked.Add(&this.AcceptedMutationCount, acceptedMutationCount) |> ignore
        Interlocked.Add(&this.ProposedMutationCount, mutationPerChain) |> ignore

    override this.Render(camera, film, aggregate, lightSampler) =
        this.AcceptedMutationCount <- 0
        this.ProposedMutationCount <- 0
        let partitioner = Partitioner.Create(0, this.NBootstrap)

        Parallel.ForEach(
            partitioner,
            (fun range _ ->
                let xs = Allocation.StackAlloc<PrimarySample>(4 + 7 * this.MaxDepth)
                let first, last = range
                for bootstrapId = first to last - 1 do
                    this.BootstrapSingleChain(bootstrapId, xs, camera, film, aggregate, lightSampler))
        )
        |> ignore

        this.B <- Array.average this.BootstrapWeights

        if this.B = 0f then
            printfn "Warning: all bootstrap samples are zero, exiting..."
        else
            let bootstrapSamplingTable = AliasTable(this.BootstrapWeights)
            let partitioner = Partitioner.Create(0, this.NChains)

            Parallel.ForEach(
                partitioner,
                (fun range _ ->
                    let xs = Allocation.StackAlloc<PrimarySample>(4 + 7 * this.MaxDepth)
                    let first, last = range
                    for chainId = first to last - 1 do
                        this.RenderSingleChain(chainId, bootstrapSamplingTable, xs, camera, film, aggregate, lightSampler))
            )
            |> ignore

            printfn $"Accepted mutation count: %d{this.AcceptedMutationCount}"
            printfn $"Proposed mutation count: %d{this.ProposedMutationCount}"
            printfn $"Acceptance rate: %f{float this.AcceptedMutationCount / float this.ProposedMutationCount}"
