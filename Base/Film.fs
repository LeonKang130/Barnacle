namespace Barnacle.Base

open System
open System.Numerics
open System.Threading.Tasks
open System.IO

type ToneMapping =
    | Identity
    | Aces
    | Gamma

type Film(resolution: int * int, toneMapping: ToneMapping) =
    let mutable _pixels = Array.create (fst resolution * snd resolution) Vector3.Zero

    member this.ToneMapping = toneMapping

    member inline this.PostProcess(x: Vector3) =
        match this.ToneMapping with
        | Identity -> x
        | Aces ->
            let a, b, c, d, e = 2.51f, 0.03f, 2.43f, 0.59f, 0.14f

            x * (a * x + b * Vector3.One)
            / (x * (c * x + d * Vector3.One) + e * Vector3.One)
        | Gamma ->
            let gamma = 1f / 2.2f
            Vector3(MathF.Pow(x.X, gamma), MathF.Pow(x.Y, gamma), MathF.Pow(x.Z, gamma))
        |> fun x -> Vector3.Clamp(x, Vector3.Zero, Vector3.One)

    member this.Resolution = resolution
    member this.ImageWidth = fst resolution
    member this.ImageHeight = snd resolution
    member this.AspectRatio = float32 (fst resolution) / float32 (snd resolution)
    
    member this.Clear() =
        Parallel.For(0, _pixels.Length - 1, fun i -> _pixels[i] <- Vector3.Zero)

    member this.SetPixel(pixelId: int * int, color: Vector3) =
        let imageX, imageY = pixelId
        _pixels[(this.ImageHeight - imageY - 1) * this.ImageWidth + imageX] <- color

    member this.Save(filename: string) =
        let toInt (x: float32) = int (255f * x + 0.5f)
        use file = File.CreateText filename
        file.Write $"P3\n{this.ImageWidth} {this.ImageHeight}\n255\n"

        _pixels
        |> Array.toSeq
        |> Seq.map this.PostProcess
        |> Seq.iter (fun x -> file.Write $"{toInt x.X} {toInt x.Y} {toInt x.Z} ")
