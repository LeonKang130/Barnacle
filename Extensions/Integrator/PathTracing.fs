#nowarn "9"
namespace Barnacle.Extensions.Integrator

open Barnacle.Base
open System
open System.Numerics

[<Sealed>]
type PathTracingIntegrator(spp: int, maxDepth: int, rrDepth: int) =
    inherit ProgressiveIntegrator(spp)
    member this.MaxDepth = maxDepth
    member this.RRDepth = rrDepth

    override this.Li(ray, aggregate, lightSampler, sampler) =
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

                if interaction.HasMaterial then
                    lightSample <- lightSampler.Sample(interaction.Position, sampler.Next1D(), sampler.Next2D())
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

                    bsdfSample <- interaction.SampleBSDF(-ray.Direction, sampler.Next1D(), sampler.Next2D())

                    if bsdfSample.eval.pdf = 0f then
                        depth <- this.MaxDepth
                    else
                        ray <- interaction.SpawnRay(bsdfSample.wi)
                        beta <- beta * bsdfSample.eval.bsdf * MathF.ReciprocalEstimate(bsdfSample.eval.pdf)

                        if depth >= this.RRDepth then
                            let rrThreshold = MathF.Min(1f, MathF.Max(beta.X, MathF.Max(beta.Y, beta.Z)))

                            if sampler.Next1D() < rrThreshold then
                                beta <- beta * MathF.ReciprocalEstimate(rrThreshold)
                            else
                                depth <- this.MaxDepth

                        depth <- depth + 1
                else
                    depth <- this.MaxDepth

        L
