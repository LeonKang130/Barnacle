namespace Barnacle.Extensions.LightSampler

open Barnacle.Base
open System
open System.Numerics
open System.Runtime.InteropServices
open System.Runtime.CompilerServices

[<Sealed>]
type UniformLightSampler(primitiveInstances: PrimitiveInstance array) =
    inherit LightSamplerBase(primitiveInstances)

    override this.Sample(p: Vector3, uSelect: float32, uLight: Vector2) =
        let mutable uSelect = uSelect * float32 this.Instances.Length
        let primitiveId = min (int uSelect) (this.Instances.Length - 1)
        uSelect <- uSelect - float32 primitiveId
        let struct (interaction, pdfSurface) =
            Unsafe.Add(&MemoryMarshal.GetArrayDataReference this.Instances, primitiveId).Sample(uSelect, uLight)
        let wo = Vector3.Normalize(p - interaction.Position)
        let cosWo = Vector3.Dot(interaction.Normal, wo)
        let dist2 = (p - interaction.Position).LengthSquared()
        {
            eval = {
                p = interaction.Position
                L = interaction.EvalEmit(wo)
                pdf = dist2 * pdfSurface / (MathF.Max(MathF.Abs(cosWo), 1e-6f) * float32 this.Instances.Length)
            }
            wi = -wo
        }
    
    override this.SampleEmit(uSelect: float32, uLight: Vector2, uEmit: Vector2) =
        let mutable uSelect = uSelect * float32 this.Instances.Length
        let primitiveId = min (int uSelect) (this.Instances.Length - 1)
        uSelect <- uSelect - float32 primitiveId
        let struct (interaction, pdfSurface) =
            Unsafe.Add(&MemoryMarshal.GetArrayDataReference this.Instances, primitiveId).Sample(uSelect, uLight)
        let sample = interaction.SampleEmit(uEmit)
        { sample with eval.pdf = pdfSurface * sample.eval.pdf / float32 this.Instances.Length }
    
    override this.Eval(p: Vector3, interaction: Interaction inref) =
        let wo = Vector3.Normalize(p - interaction.Position)
        let cosWo = Vector3.Dot(interaction.Normal, wo)
        let pdfSurface = interaction.inst.EvalPDF(&interaction)
        let dist2 = (p - interaction.Position).LengthSquared()
        {
            p = interaction.Position
            L = interaction.EvalEmit(wo)
            pdf = dist2 * pdfSurface / (MathF.Max(MathF.Abs(cosWo), 1e-6f) * float32 this.Instances.Length)
        }
