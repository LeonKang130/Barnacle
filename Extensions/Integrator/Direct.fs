namespace Barnacle.Extensions.Integrator

open Barnacle.Base
open System.Numerics

[<Sealed>]
type DirectIntegrator(spp: int) =
    inherit ProgressiveIntegrator(spp)

    override this.Li(ray, aggregate, lightSampler, sampler) =
        let mutable t = infinityf
        let mutable interaction = Unchecked.defaultof<Interaction>
        let mutable L = Vector3.Zero

        if aggregate.Intersect(&ray, &interaction, &t) then
            if interaction.HasLight then
                L <- L + interaction.EvalEmit(-ray.Direction)

            if interaction.HasMaterial then
                let lightSample =
                    lightSampler.Sample(interaction.Position, sampler.Next1D(), sampler.Next2D())

                let shadowRay = interaction.SpawnRay(lightSample.wi)
                let lightDistance = (lightSample.eval.p - interaction.Position).Length()
                let occluded = aggregate.Intersect(&shadowRay, lightDistance - 1e-3f)

                if not occluded then
                    let bsdfEval: BSDFEval = interaction.EvalBSDF(-ray.Direction, lightSample.wi)
                    L <- Vector3.FusedMultiplyAdd(bsdfEval.bsdf, lightSample.eval.L * (1f / lightSample.eval.pdf), L)

                let bsdfSample = interaction.SampleBSDF(-ray.Direction, sampler.Next2D())
                let ray = interaction.SpawnRay(bsdfSample.wi)
                interaction <- Unchecked.defaultof<Interaction>
                t <- infinityf

                if aggregate.Intersect(&ray, &interaction, &t) && interaction.HasLight then
                    let lightEval = lightSampler.Eval(ray.Origin, &interaction)
                    L <- Vector3.FusedMultiplyAdd(bsdfSample.eval.bsdf, lightEval.L * (1f / lightEval.pdf), L)

        L
