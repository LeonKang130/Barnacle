namespace Barnacle.Util

open System
open System.Numerics

module Quantization =
    let private SQRT5 = MathF.Sqrt(5f)
    let private PHI = (1f + SQRT5) / 2f
    
    type private Matrix2x2 =
        {
            M11: float32
            M12: float32
            M21: float32
            M22: float32
        }

    let inline private MatMul(m: Matrix2x2, x: Vector2) =
        Vector2(m.M11 * x.X + m.M12 * x.Y, m.M21 * x.X + m.M22 * x.Y)
    
    let inline private SphericalFibonacciMap (n: uint, i: uint) =
        let inline frac x = x - MathF.Floor(x)
        let n' = float32 n
        let i' = float32 i
        let phi = 2f * MathF.PI * frac(i' * PHI)
        let cosTheta = 1f - MathF.FusedMultiplyAdd(2f, i', 1f) / n'
        let sinTheta = MathF.Sqrt(MathF.FusedMultiplyAdd(-cosTheta, cosTheta, 1f))
        let struct (sinPhi, cosPhi) = MathF.SinCos(phi)
        Vector3(sinTheta * cosPhi, sinTheta * sinPhi, cosTheta)

    let inline private InverseSphericalFibonacciMap (n: uint, p: Vector3) =
        let inline floor2 (x: Vector2) = Vector2(MathF.Floor(x.X), MathF.Floor(x.Y))
        let inline frac2 (x: Vector2) = x - floor2 x
        let n' = float32 n
        let k = MathF.Max(2f, MathF.Floor(MathF.Log2(n' * MathF.PI * SQRT5 * MathF.FusedMultiplyAdd(-p.Z, p.Z, 1f)) / MathF.Log2(PHI + 1f)))
        let Fk = MathF.Pow(PHI, k) / SQRT5
        let F = Vector2(MathF.Round(Fk), MathF.Round(Fk * PHI))
        let ka = (2f / n') * F
        let kb = 2f * MathF.PI * (frac2((F + Vector2.One) * PHI) - Vector2(PHI - 1f))
        let det = ka.Y * kb.X - ka.X * kb.Y
        let invB = {
            M11 = ka.Y / det
            M21 = -ka.X / det
            M12 = kb.Y / det
            M22 = -kb.X / det
        }
        let c = floor2(MatMul(invB, Vector2(MathF.Atan2(p.Y, p.X), p.Z - (1f - 1f / n'))))
        let mutable d = infinityf
        let mutable j = 0u
        for s = 0 to 3 do
            let uv = Vector2(float32 (s &&& 1), float32 (s >>> 1))
            let i = uint (Single.Clamp(Vector2.Dot(F, uv + c), 0f, n' - 1f))
            let p' = SphericalFibonacciMap(n, i)
            let d' = Vector3.DistanceSquared(p, p')
            if d' < d then
                d <- d'
                j <- i
        j
    
    let EncodeUnitVector (v: Vector3) =
        InverseSphericalFibonacciMap(1u <<< 31, v)

    let DecodeUnitVector (i: uint) =
        SphericalFibonacciMap(1u <<< 31, i)