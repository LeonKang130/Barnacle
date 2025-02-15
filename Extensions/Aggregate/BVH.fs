#nowarn "9"
namespace Barnacle.Extensions.Aggregate

open Barnacle.Base
open System
open Microsoft.FSharp.NativeInterop

module private TLASTraversal =
    let inline stackalloc<'a when 'a: unmanaged> (length: int): Span<'a> =
        let p = NativePtr.stackalloc<'a> length |> NativePtr.toVoidPtr
        Span<'a>(p, length)

type BVHAggregate(instances: PrimitiveInstance array) =
    inherit PrimitiveAggregate()
    member val private BVHNodes =
        let builder = BVHBuilder.Default
        if builder.Config.MaxDepth > 128 then
            failwith "BVH depth potentially exceeds 128"
        builder.Build(instances.AsSpan(), _.Bounds) with get
    override this.Intersect(ray: Ray inref, t: float32) =
        let traversalStack = TLASTraversal.stackalloc<int> 128
        traversalStack[0] <- 0
        let mutable stackTop = 1
        let mutable hit = false
        while not hit && stackTop <> 0 do
            stackTop <- stackTop - 1
            let i = traversalStack[stackTop]
            let node = this.BVHNodes[i]
            if node.Bounds.Intersect(&ray, t) then
                if node.IsLeaf then
                    let mutable instanceId = node.InstanceOffset
                    while not hit && instanceId < node.InstanceOffset + node.InstanceCount do
                        hit <- instances[instanceId].Intersect(&ray, t)
                        instanceId <- instanceId + 1
                else
                    if ray.Direction[node.SplitAxis] > 0f then
                        traversalStack[stackTop] <- node.RightChild
                        traversalStack[stackTop + 1] <- i + 1
                    else
                        traversalStack[stackTop] <- i + 1
                        traversalStack[stackTop + 1] <- node.RightChild
                    stackTop <- stackTop + 2
        hit
    override this.Intersect(ray: Ray inref, interaction: Interaction outref, t: float32 byref) =
        let traversalStack = TLASTraversal.stackalloc<int> 128
        traversalStack[0] <- 0
        let mutable stackTop = 1
        let mutable hit = false
        while stackTop <> 0 do
            stackTop <- stackTop - 1
            let i = traversalStack[stackTop]
            let node = this.BVHNodes[i]
            if node.Bounds.Intersect(&ray, t) then
                if node.IsLeaf then
                    for instanceId = node.InstanceOffset to node.InstanceOffset + node.InstanceCount - 1 do
                        hit <- instances[instanceId].Intersect(&ray, &interaction, &t) || hit
                else
                    if ray.Direction[node.SplitAxis] > 0f then
                        traversalStack[stackTop] <- node.RightChild
                        traversalStack[stackTop + 1] <- i + 1
                    else
                        traversalStack[stackTop] <- i + 1
                        traversalStack[stackTop + 1] <- node.RightChild
                    stackTop <- stackTop + 2
        hit
