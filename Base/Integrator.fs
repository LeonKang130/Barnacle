namespace Barnacle.Base

open System
open System.Numerics
open System.Threading.Tasks

[<AbstractClass>]
type IntegratorBase() =
    abstract member Li: Ray inref * PrimitiveAggregate * LightSamplerBase * Sampler byref -> Vector3
    abstract member Render: CameraBase * Film * PrimitiveAggregate * LightSamplerBase -> unit

[<AbstractClass>]
type ProgressiveIntegrator(spp: int, tileSize: int) =
    inherit IntegratorBase()
    let mutable _frameId = 0
    new(spp: int) = ProgressiveIntegrator(spp, 16)
    member this.SamplePerPixel = spp
    member this.TileSize = tileSize    
    abstract member RenderTile: int * int * CameraBase * Film * PrimitiveAggregate * LightSamplerBase -> unit
    default this.RenderTile(tx: int, ty: int, camera: CameraBase, film: Film, aggregate: PrimitiveAggregate, lightSampler: LightSamplerBase) =
        let tileXFirst = tx * this.TileSize
        let tileYFirst = ty * this.TileSize
        let tileXLast = min ((tx + 1) * this.TileSize) film.ImageWidth
        let tileYLast = min ((ty + 1) * this.TileSize) film.ImageHeight
        for x = tileXFirst to tileXLast - 1 do
            for y = tileYFirst to tileYLast - 1 do
                let mutable accum = Vector3.Zero
                for sampleId = 0 to this.SamplePerPixel - 1 do
                    let mutable sampler =
                        Sampler(uint x, uint y, uint (_frameId * this.SamplePerPixel + sampleId))
                    let ray, pdf =
                        camera.GeneratePrimaryRay(film.Resolution, (x, y), sampler.Next2D(), sampler.Next2D())
                    let mutable radiance = this.Li(&ray, aggregate, lightSampler, &sampler) / pdf
                    radiance.X <- if Single.IsNaN radiance.X then 0f else radiance.X
                    radiance.Y <- if Single.IsNaN radiance.Y then 0f else radiance.Y
                    radiance.Z <- if Single.IsNaN radiance.Z then 0f else radiance.Z
                    accum <- accum + (1f / float32 this.SamplePerPixel) * radiance

                film.SetPixel((x, y), accum)
    
    override this.Render(camera: CameraBase, film: Film, aggregate: PrimitiveAggregate, lightSampler: LightSamplerBase) =
        seq {
            let tileXCount = (film.ImageWidth + this.TileSize - 1) / this.TileSize
            let tileYCount = (film.ImageHeight + this.TileSize - 1) / this.TileSize
            for tx = 0 to tileXCount - 1 do
                for ty = 0 to tileYCount - 1 do
                    yield Task.Run(fun () -> this.RenderTile(tx, ty, camera, film, aggregate, lightSampler))
        }
        |> Seq.toArray
        |> Task.WhenAll
        |> Async.AwaitTask
        |> Async.RunSynchronously

        _frameId <- _frameId + 1
