#nowarn "9"
namespace Barnacle.Extensions.Integrator

open Barnacle.Base
open System.Numerics

type PathTracingIntegrator(spp: int, maxDepth: int, rrDepth: int, rrThreshold: float32) =
    inherit ProgressiveIntegrator(spp)
    new(spp) = PathTracingIntegrator(spp, 8, 5, 0.95f)
    new(spp, maxDepth) = PathTracingIntegrator(spp, maxDepth, 5, 0.95f)
    member this.MaxDepth = maxDepth
    member this.RRDepth = rrDepth
    member this.RRThreshold = rrThreshold
    override this.Li(ray: Ray inref, aggregate: PrimitiveAggregate, lightSampler: LightSamplerBase, sampler: Sampler byref) =
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
                    L <- L + beta * lightSample.eval.L * bsdfSample.eval.pdf / (bsdfSample.eval.pdf + lightSample.eval.pdf)
                if interaction.HasMaterial then
                    lightSample <- lightSampler.Sample(interaction.Position, sampler.Next1D(), sampler.Next2D())
                    let shadowRay = interaction.SpawnRay(lightSample.wi)
                    let lightDistance = (lightSample.eval.p - interaction.Position).Length()
                    let occluded = aggregate.Intersect(&shadowRay, lightDistance - 1e-3f)
                    if not occluded then
                        bsdfSample.eval <- interaction.EvalBSDF(-ray.Direction, lightSample.wi)
                        L <- L + beta * bsdfSample.eval.bsdf * lightSample.eval.L / (lightSample.eval.pdf + bsdfSample.eval.pdf)
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
        L

