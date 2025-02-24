namespace Barnacle.Extensions.Scene

open Barnacle.Base
open Barnacle.Extensions.Aggregate
open Barnacle.Extensions.LightSampler

[<AutoOpen>]
module SceneRenderExtensions =
    type Scene with
        member this.Render(t: float32, filename: string) =
            this.Film.Clear()
            let instances = this.Traverse(t)
            let aggregate = BVHAggregate(instances)
            let lightSampler = UniformLightSampler(instances)
            let stopwatch = System.Diagnostics.Stopwatch.StartNew()
            this.Integrator.Render(this.Camera, this.Film, aggregate, lightSampler)
            stopwatch.Stop()
            printfn $"Render time: %f{stopwatch.Elapsed.TotalSeconds} seconds"
            this.Film.Save filename
