namespace Barnacle.Extensions.Integrator

open Barnacle.Base
open System.Numerics

type DirectIntegrator(spp: int) =
    inherit ProgressiveIntegrator(spp)

    override this.Li
        (ray: Ray inref, aggregate: PrimitiveAggregate, lightSampler: LightSamplerBase, sampler: Sampler byref)
        =
        let mutable t = infinityf
        let mutable interaction = Unchecked.defaultof<Interaction>
        let mutable L = Vector3.Zero

        if aggregate.Intersect(&ray, &interaction, &t) then
            if interaction.HasLight then
                L <- L + interaction.EvalEmit(-ray.Direction)

            if interaction.HasMaterial then
                let lightSample = lightSampler.Sample(interaction.Position, sampler.Next2D())
                let shadowRay = interaction.SpawnRay(lightSample.wi)
                let lightDistance = (lightSample.eval.p - interaction.Position).Length()
                let occluded = aggregate.Intersect(&shadowRay, lightDistance - 1e-3f)

                if not occluded then
                    let bsdfEval: BSDFEval = interaction.EvalBSDF(-ray.Direction, lightSample.wi)
                    let misWeight = lightSample.eval.pdf / (lightSample.eval.pdf + bsdfEval.pdf)
                    L <- L + misWeight * bsdfEval.bsdf * lightSample.eval.L / lightSample.eval.pdf

                let bsdfSample = interaction.SampleBSDF(-ray.Direction, sampler.Next2D())
                let ray = interaction.SpawnRay(bsdfSample.wi)
                interaction <- Unchecked.defaultof<Interaction>
                t <- infinityf

                if aggregate.Intersect(&ray, &interaction, &t) && interaction.HasLight then
                    let lightEval = lightSampler.Eval(ray.Origin, &interaction)
                    let misWeight = bsdfSample.eval.pdf / (lightEval.pdf + bsdfSample.eval.pdf)
                    L <- L + misWeight * bsdfSample.eval.bsdf * lightEval.L / bsdfSample.eval.pdf

        L
