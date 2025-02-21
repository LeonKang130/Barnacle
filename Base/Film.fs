namespace Barnacle.Base

open System
open System.Runtime.CompilerServices
open System.Numerics
open System.Runtime.InteropServices
open System.Threading.Tasks
open SixLabors.ImageSharp

type ToneMapping =
    | Identity
    | Aces
    | Gamma

type Film(resolution: struct (int * int), toneMapping: ToneMapping) =
    let struct (imageWidth, imageHeight) = resolution
    member val Pixels = Array.zeroCreate<Vector3> (imageWidth * imageHeight) with get

    member this.ToneMapping = toneMapping

    member inline this.PostProcess(x: Vector3) =
        match this.ToneMapping with
        | Identity -> x
        | Aces ->
            x * (2.51f * x + Vector3(0.03f))
            / (x * (2.43f * x + Vector3(0.59f)) + Vector3(0.14f))
        | Gamma ->
            let gamma = 1f / 2.2f
            Vector3(MathF.Pow(x.X, gamma), MathF.Pow(x.Y, gamma), MathF.Pow(x.Z, gamma))
        |> fun x -> Vector3.Clamp(x, Vector3.Zero, Vector3.One)

    member this.Resolution = struct (imageWidth, imageHeight)
    member val ImageWidth = imageWidth with get
    member val ImageHeight = imageHeight with get
    member val AspectRatio = float32 imageWidth / float32 imageHeight with get

    member inline this.Clear() =
        Parallel.For(0, this.Pixels.Length - 1, (fun i -> this.Pixels[i] <- Vector3.Zero))
        |> ignore

    member inline this.SetPixel(pixelId: struct (int * int), color: Vector3) =
        let struct (imageX, imageY) = pixelId
        let index = (this.ImageHeight - imageY - 1) * this.ImageWidth + imageX
        let pixels = &MemoryMarshal.GetArrayDataReference this.Pixels
        let pixel = &Unsafe.Add(&pixels, index)
        pixel <- color

    member inline this.Accumulate(pixelId: struct (int * int), color: Vector3) =
        let struct (imageX, imageY) = pixelId
        let index = (this.ImageHeight - imageY - 1) * this.ImageWidth + imageX
        let pixels = &MemoryMarshal.GetArrayDataReference this.Pixels
        let pixel = &Unsafe.Add(&pixels, index)
        pixel <- pixel + color

    member this.Save(filename: string) =
        use image = new Image<PixelFormats.Rgba32>(this.ImageWidth, this.ImageHeight)

        image.ProcessPixelRows(fun accessor ->
            for y = 0 to accessor.Height - 1 do
                let row = accessor.GetRowSpan(y)

                for x = 0 to row.Length - 1 do
                    let pixel = this.Pixels[y * this.ImageWidth + x]
                    row[x] <- PixelFormats.Rgba32(this.PostProcess(pixel)))

        image.Save(filename)
