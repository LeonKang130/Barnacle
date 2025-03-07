﻿namespace Barnacle.Base

open System
open System.Numerics
open System.Collections.Concurrent
open System.Threading.Tasks

[<AbstractClass>]
type IntegratorBase() =
    abstract member Li: Ray inref * PrimitiveAggregate * LightSamplerBase * Sampler byref -> Vector3
    abstract member Render: CameraBase * Film * PrimitiveAggregate * LightSamplerBase -> unit

[<AbstractClass>]
type ProgressiveIntegrator(spp: int, tileSize: int) =
    inherit IntegratorBase()
    new(spp: int) = ProgressiveIntegrator(spp, 16)
    member val FrameId = 0 with get, set
    member val SamplePerPixel = spp with get
    member val TileSize = tileSize with get
    override this.Li(_, _, _, _) = Vector3.Zero
    abstract member RenderTile: int * int * CameraBase * Film * PrimitiveAggregate * LightSamplerBase -> unit
    default this.RenderTile
        (tx: int, ty: int, camera: CameraBase, film: Film, aggregate: PrimitiveAggregate, lightSampler: LightSamplerBase) =
        let tileXFirst = tx * this.TileSize
        let tileYFirst = ty * this.TileSize
        let tileXLast = min ((tx + 1) * this.TileSize) film.ImageWidth
        let tileYLast = min ((ty + 1) * this.TileSize) film.ImageHeight
        let invSamplePerPixel = MathF.ReciprocalEstimate(float32 this.SamplePerPixel)

        for imageY = tileYFirst to tileYLast - 1 do
            for imageX = tileXFirst to tileXLast - 1 do
                let mutable accum = Vector3.Zero

                for sampleId = 0 to this.SamplePerPixel - 1 do
                    let mutable sampler =
                        Sampler(uint imageX, uint imageY, uint (this.FrameId * this.SamplePerPixel + sampleId))

                    let struct (ray, pdf) =
                        camera.GeneratePrimaryRay(film.Resolution, struct (imageX, imageY), sampler.Next2D(), sampler.Next2D())

                    let radiance = this.Li(&ray, aggregate, lightSampler, &sampler) * MathF.ReciprocalEstimate(pdf)
                    accum <- Vector3.FusedMultiplyAdd(Vector3(invSamplePerPixel), radiance, accum)

                film.SetPixel(struct (imageX, imageY), accum)

    override this.Render(camera: CameraBase, film: Film, aggregate: PrimitiveAggregate, lightSampler: LightSamplerBase) =
        let tileXCount = (film.ImageWidth + this.TileSize - 1) / this.TileSize
        let tileYCount = (film.ImageHeight + this.TileSize - 1) / this.TileSize
        let partitioner = Partitioner.Create(0, tileXCount * tileYCount)
        Parallel.ForEach(partitioner, (fun range _ ->
            let first, last = range
            for tileId = first to last - 1 do
                this.RenderTile(tileId % tileXCount, tileId / tileXCount, camera, film, aggregate, lightSampler))
        ) |> ignore
        this.FrameId <- this.FrameId + 1
