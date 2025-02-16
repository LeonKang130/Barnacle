namespace Barnacle.Base

open System
open System.Numerics
open System.Threading
open System.Threading.Tasks
open System.IO

type ToneMapping =
    | Identity
    | Aces
    | Gamma

type Film(resolution: int * int, toneMapping: ToneMapping) =
    member val Pixels = Array.zeroCreate<Vector3> (fst resolution * snd resolution) with get

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

    member this.Resolution = resolution
    member this.ImageWidth = fst resolution
    member this.ImageHeight = snd resolution
    member this.AspectRatio = float32 (fst resolution) / float32 (snd resolution)
    
    member inline this.Clear() =
        Parallel.For(0, this.Pixels.Length - 1, fun i -> this.Pixels[i] <- Vector3.Zero) |> ignore

    member inline this.SetPixel(pixelId: int * int, color: Vector3) =
        let imageX, imageY = pixelId
        let index = (this.ImageHeight - imageY - 1) * this.ImageWidth + imageX
        this.Pixels[index] <- color

    member inline this.Accumulate(pixelId: int * int, color: Vector3) =
        let imageX, imageY = pixelId
        let index = (this.ImageHeight - imageY - 1) * this.ImageWidth + imageX
        this.Pixels[index] <- this.Pixels[index] + color
    
    member inline this.Save(filename: string) =
        let toInt (x: float32) = int (255f * x + 0.5f)
        use file = File.CreateText filename
        file.Write $"P3\n{this.ImageWidth} {this.ImageHeight}\n255\n"

        this.Pixels
        |> Array.toSeq
        |> Seq.map this.PostProcess
        |> Seq.iter (fun x -> file.Write $"{toInt x.X} {toInt x.Y} {toInt x.Z} ")
