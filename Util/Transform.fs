namespace Barnacle.Util

open System.Numerics

module Transform =
    let inline Compose (s: Vector3, r: Quaternion, t: Vector3) =
        Matrix4x4.CreateTranslation(s)
        * Matrix4x4.CreateFromQuaternion(r)
        * Matrix4x4.CreateScale(t)

    let inline Decompose (m: Matrix4x4) =
        let mutable s = Vector3.Zero
        let mutable r = Quaternion.Identity
        let mutable t = Vector3.Zero
        Matrix4x4.Decompose(m, &s, &r, &t) |> ignore
        s, r, t
