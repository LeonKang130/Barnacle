namespace Barnacle.Extensions.Integrator

open Barnacle.Base
open System.Numerics

type NormalIntegrator(spp: int) =
    inherit ProgressiveIntegrator(spp)
    override this.Li(ray: Ray inref, aggregate: PrimitiveAggregate, _, sampler: Sampler byref) =
        let mutable t = infinityf
        let mutable interaction = Unchecked.defaultof<Interaction>
        if aggregate.Intersect(&ray, &interaction, &t) then
            0.5f * (interaction.Normal + Vector3.One)
        else
            Vector3.Zero

