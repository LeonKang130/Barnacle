#nowarn "9"
namespace Barnacle.Extensions.Aggregate

open Barnacle.Base
open System
open System.Runtime.CompilerServices
open Microsoft.FSharp.NativeInterop

module private TLASTraversal =
    let inline stackalloc<'a when 'a: unmanaged> (length: int): 'a byref =
        let p = NativePtr.stackalloc<'a> length
        NativePtr.toByRef p

    let inline stackPush<'a when 'a: unmanaged> (stack: 'a byref, stackTop: int byref, value: 'a) =
        Unsafe.Add(&stack, stackTop) <- value
        stackTop <- stackTop + 1
    
    let inline stackPop<'a when 'a: unmanaged> (stack: 'a byref, stackTop: int byref) : 'a =
        stackTop <- stackTop - 1
        Unsafe.Add(&stack, stackTop)

type BVHAggregate(instances: PrimitiveInstance array) =
    inherit PrimitiveAggregate()
    member val private BVHNodes =
        let builder = BVHBuilder.Default
        if builder.Config.MaxDepth > 128 then
            failwith "BVH depth potentially exceeds 128"
        builder.Build(instances.AsSpan(), _.Bounds) with get
    override this.Intersect(ray: Ray inref, t: float32) =
        let traversalStack = &TLASTraversal.stackalloc<int> 128
        let mutable stackTop = 0
        TLASTraversal.stackPush(&traversalStack, &stackTop, 0)
        let mutable hit = false
        while not hit && stackTop <> 0 do
            let i = TLASTraversal.stackPop(&traversalStack, &stackTop)
            let node = this.BVHNodes[i]
            if node.Bounds.Intersect(&ray, t) then
                if node.IsLeaf then
                    let mutable instanceId = node.InstanceOffset
                    while not hit && instanceId < node.InstanceOffset + node.InstanceCount do
                        hit <- instances[instanceId].Intersect(&ray, t)
                        instanceId <- instanceId + 1
                else
                    if ray.Direction[node.SplitAxis] > 0f then
                        TLASTraversal.stackPush(&traversalStack, &stackTop, node.RightChild)
                        TLASTraversal.stackPush(&traversalStack, &stackTop, i + 1)
                    else
                        TLASTraversal.stackPush(&traversalStack, &stackTop, i + 1)
                        TLASTraversal.stackPush(&traversalStack, &stackTop, node.RightChild)
        hit
    override this.Intersect(ray: Ray inref, interaction: Interaction outref, t: float32 byref) =
        let traversalStack = &TLASTraversal.stackalloc<int> 128
        let mutable stackTop = 0
        TLASTraversal.stackPush(&traversalStack, &stackTop, 0)
        let mutable hit = false
        while stackTop <> 0 do
            let i = TLASTraversal.stackPop(&traversalStack, &stackTop)
            let node = this.BVHNodes[i]
            if node.Bounds.Intersect(&ray, t) then
                if node.IsLeaf then
                    for instanceId = node.InstanceOffset to node.InstanceOffset + node.InstanceCount - 1 do
                        hit <- instances[instanceId].Intersect(&ray, &interaction, &t) || hit
                else
                    if ray.Direction[node.SplitAxis] > 0f then
                        TLASTraversal.stackPush(&traversalStack, &stackTop, node.RightChild)
                        TLASTraversal.stackPush(&traversalStack, &stackTop, i + 1)
                    else
                        TLASTraversal.stackPush(&traversalStack, &stackTop, i + 1)
                        TLASTraversal.stackPush(&traversalStack, &stackTop, node.RightChild)
        hit
