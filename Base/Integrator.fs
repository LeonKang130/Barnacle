namespace Barnacle.Base

open System.Numerics
open System.Threading.Tasks

[<AbstractClass>]
type IntegratorBase() =
    abstract member Li: Ray inref * PrimitiveAggregate * LightSamplerBase * Sampler byref -> Vector3
    abstract member Render: CameraBase * Film * PrimitiveAggregate * LightSamplerBase -> unit

[<AbstractClass>]
type ProgressiveIntegrator(spp: int) =
    inherit IntegratorBase()
    let mutable _frameId = 0
    member this.SamplePerPixel = spp

    override this.Render(camera: CameraBase, film: Film, aggregate: PrimitiveAggregate, lightSampler: LightSamplerBase) =
        let inline renderRow (y: int) =
            for x = 0 to film.ImageWidth - 1 do
                let mutable radiance = Vector3.Zero

                for sampleId = 0 to this.SamplePerPixel - 1 do
                    let mutable sampler =
                        Sampler(uint x, uint y, uint (_frameId * this.SamplePerPixel + sampleId))

                    let ray, pdf =
                        camera.GeneratePrimaryRay(film.Resolution, (x, y), sampler.Next2D(), sampler.Next2D())

                    radiance <-
                        radiance
                        + (1f / float32 this.SamplePerPixel * pdf) * this.Li(&ray, aggregate, lightSampler, &sampler)

                film.SetPixel((x, y), radiance)

        seq { 0 .. film.ImageHeight - 1 }
        |> Seq.map (fun y -> Task.Run(fun () -> renderRow y))
        |> Seq.toArray
        |> Task.WhenAll
        |> Async.AwaitTask
        |> Async.RunSynchronously

        _frameId <- _frameId + 1
