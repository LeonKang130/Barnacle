namespace Barnacle.Extensions.LightSampler

open Barnacle.Base
open System
open System.Numerics

type UniformLightSampler(primitiveInstances: PrimitiveInstance array) =
    inherit LightSamplerBase(primitiveInstances)

    override this.Sample(p: Vector3, uLight: Vector2) =
        let mutable uLight = uLight
        uLight.X <- uLight.X * float32 this.Instances.Length
        let primitiveId = min (int uLight.X) (this.Instances.Length - 1)
        uLight.X <- uLight.X - float32 primitiveId
        let interaction, pdfSurface = this.Instances[primitiveId].Sample(uLight)
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
