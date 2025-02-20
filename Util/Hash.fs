namespace Barnacle.Util

open System.Runtime.CompilerServices

module Hash =
    let inline XXHash32Two (x: uint, y: uint) =
        let prime32_2 = 2246822519u
        let prime32_3 = 3266489917u
        let prime32_4 = 668265263u
        let prime32_5 = 374761393u
        let mutable h32 = y + prime32_5 + x * prime32_3
        h32 <- prime32_4 * ((h32 <<< 17) ||| (h32 >>> 15))
        h32 <- prime32_2 * (h32 ^^^ (h32 >>> 15))
        h32 <- prime32_3 * (h32 ^^^ (h32 >>> 13))
        h32 ^^^ (h32 >>> 16)

    let inline XXHash32Three (x: uint, y: uint, z: uint) =
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

    let inline LCG (seed: uint byref) =
        seed <- 0x00269ec3u + seed * 0x000343fdu
        Unsafe.BitCast<uint, float32>((seed >>> 9) ||| 0x3f800000u) - 1f
