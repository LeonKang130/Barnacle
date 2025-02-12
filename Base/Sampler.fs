namespace Barnacle.Base

open System.Numerics
open System.Runtime.CompilerServices

module RNG =
    let inline xxhash32 (x: uint) (y: uint) (z: uint) =
        let prime32_2, prime32_3, prime32_4, prime32_5 =
            2246822519u, 3266489917u, 668265263u, 374761393u

        let mutable h32 = z + prime32_5 + x * prime32_3
        h32 <- prime32_4 * ((h32 <<< 17) ||| (h32 >>> (32 - 17)))
        h32 <- h32 + y * prime32_3
        h32 <- prime32_4 * ((h32 <<< 17) ||| (h32 >>> (32 - 17)))
        h32 <- prime32_2 * (h32 ^^^ (h32 >>> 15))
        h32 <- prime32_3 * (h32 ^^^ (h32 >>> 13))
        h32 ^^^ (h32 >>> 16)

    let inline lcg (seed: uint byref) =
        seed <- 0x00269ec3u + seed * 0x000343fdu
        Unsafe.BitCast<uint, float32>((seed >>> 9) ||| 0x3f800000u) - 1f

[<Struct>]
type Sampler =
    val mutable private _seed: uint
    new(seed: uint) = { _seed = seed }
    new(x: uint, y: uint, z: uint) = { _seed = RNG.xxhash32 x y z }
    member this.LoadState(state: uint) = this._seed <- state
    member this.StoreState(state: uint byref) = state <- this._seed
    member this.StoreState() = this._seed
    member this.Next1D() = RNG.lcg (&this._seed)
    member this.Next2D() = Vector2(this.Next1D(), this.Next1D())

    member this.Next3D() =
        Vector3(this.Next1D(), this.Next1D(), this.Next1D())
