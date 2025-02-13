namespace Barnacle.Base

open System
open System.Numerics
open Microsoft.FSharp.Core

module internal Util =
    let inline Compose(s: Vector3, r: Quaternion, t: Vector3) =
        Matrix4x4.CreateTranslation(s) * Matrix4x4.CreateFromQuaternion(r) * Matrix4x4.CreateScale(t)
    let inline Decompose(m: Matrix4x4) =
        let mutable s = Vector3.Zero
        let mutable r = Quaternion.Identity
        let mutable t = Vector3.Zero
        Matrix4x4.Decompose(m, &s, &r, &t) |> ignore
        s, r, t

[<Struct>]
type KeyFrame =
    val mutable Time: float32
    val mutable Matrix: Matrix4x4
    new(time: float32, matrix: Matrix4x4) = { Time = time; Matrix = matrix }
    static member inline Interpolate(a: KeyFrame inref, b: KeyFrame inref, t: float32) =
        let ratio = (t - a.Time) / (b.Time - a.Time)
        let t1, r1, s1 = Util.Decompose(a.Matrix)
        let t2, r2, s2 = Util.Decompose(b.Matrix)
        let translation = Vector3.Lerp(t1, t2, ratio)
        let rotation = Quaternion.Slerp(r1, r2, ratio)
        let scale = Vector3.Lerp(s1, s2, ratio)
        Util.Compose(scale, rotation, translation)

type Transform(keyFrames: KeyFrame[]) =
    do if keyFrames.Length = 0 then raise (ArgumentException "KeyFrames cannot be empty")
    new() = Transform([| KeyFrame(0f, Matrix4x4.Identity) |])
    new(m: Matrix4x4) = Transform([| KeyFrame(0f, m) |])
    member this.KeyFrames = keyFrames
    member this.Eval(t: float32) =
        this.KeyFrames
        |> Array.tryFindIndexBack (fun k -> k.Time <= t)
        |> (function
            | Some prevFrameIndex when prevFrameIndex = this.KeyFrames.Length - 1 -> this.KeyFrames[prevFrameIndex].Matrix
            | Some prevFrameIndex ->
                let prevFrame = this.KeyFrames[prevFrameIndex]
                let nextFrame = this.KeyFrames[prevFrameIndex + 1]
                KeyFrame.Interpolate(&prevFrame, &nextFrame, t)
            | None -> this.KeyFrames[0].Matrix)
    
type Node(transform: Transform, children: Node[], instances: PrimitiveInstance[]) =
    new(transform: Transform, instances: PrimitiveInstance[]) = Node(transform, [||], instances)
    new(transform: Transform, children: Node[]) = Node(transform, children, [||])
    member this.Transform = transform
    member this.Children = children
    member this.Instances = instances
    member this.Traverse(t: float32, nodeToWorld: Matrix4x4) =
        seq {
            let objectToWorld = this.Transform.Eval(t) * nodeToWorld
            for instance in this.Instances do
                instance.UpdateTransform(objectToWorld)
                yield instance
            for child in this.Children do
                yield! child.Traverse(t, objectToWorld)
        }

type Scene(root: Node) =
    member this.Root = root
    member this.Traverse(t: float32) = this.Root.Traverse(t, Matrix4x4.Identity) |> Seq.toArray