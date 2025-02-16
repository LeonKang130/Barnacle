namespace Barnacle.Base

open System.Numerics
open System.Runtime.CompilerServices

[<Struct>]
type Sampler =
    val mutable state: uint
    new(x: uint) = { state = Sampler.XXHash32(x, 0xdeadbeefu) }
    new(x: uint, y: uint) = { state = Sampler.XXHash32(x, y) }
    new(x: uint, y: uint, z: uint) = { state = Sampler.XXHash32(x, y, z) }
    member inline this.LoadState(state: uint) = this.state <- state
    member inline this.StoreState(state: uint byref) = state <- this.state
    member inline this.StoreState() = this.state
    member inline this.Next1D() = Sampler.LCG &this.state
    member inline this.Next2D() = Vector2(this.Next1D(), this.Next1D())
    static member XXHash32(x: uint, y: uint) =
        let prime32_2 = 2246822519u
        let prime32_3 = 3266489917u
        let prime32_4 = 668265263u
        let prime32_5 = 374761393u
        let mutable h32 = y + prime32_5 + x * prime32_3
        h32 <- prime32_4 * ((h32 <<< 17) ||| (h32 >>> 15))
        h32 <- prime32_2 * (h32 ^^^ (h32 >>> 15))
        h32 <- prime32_3 * (h32 ^^^ (h32 >>> 13))
        h32 ^^^ (h32 >>> 16)
    static member XXHash32(x: uint, y: uint, z: uint) =
        let prime32_2 = 2246822519u
        let prime32_3 = 3266489917u
        let prime32_4 = 668265263u
        let prime32_5 = 374761393u
        let mutable h32 = z + prime32_5 + x * prime32_3
        h32 <- prime32_4 * ((h32 <<< 17) ||| (h32 >>> 15))
        h32 <- h32 + y * prime32_3
        h32 <- prime32_4 * ((h32 <<< 17) ||| (h32 >>> 15))
        h32 <- prime32_2 * (h32 ^^^ (h32 >>> 15))
        h32 <- prime32_3 * (h32 ^^^ (h32 >>> 13))
        h32 ^^^ (h32 >>> 16)
    static member LCG(seed: uint byref) =
        seed <- 0x00269ec3u + seed * 0x000343fdu
        Unsafe.BitCast<uint, float32>((seed >>> 9) ||| 0x3f800000u) - 1f
