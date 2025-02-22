namespace Barnacle.Extensions.Aggregate

open Barnacle.Util
open Barnacle.Base
open System

type BVHAggregate(instances: PrimitiveInstance array) =
    inherit PrimitiveAggregate()
    member val private BVHNodes = BVHNode.Build(instances.AsSpan(), _.Bounds) with get

    override this.Intersect(ray, t) =
        let traversalStack = Allocation.StackAlloc<int> 128
        let mutable stackTop = 0
        Allocation.StackPush(traversalStack, &stackTop, 0)
        let mutable hit = false

        while not hit && stackTop <> 0 do
            let i = Allocation.StackPop(traversalStack, &stackTop)
            let node = this.BVHNodes[i]

            if node.Bounds.Intersect(&ray, t) then
                if node.IsLeaf then
                    let mutable instanceId = node.InstanceOffset

                    while not hit && instanceId < node.InstanceOffset + node.InstanceCount do
                        hit <- instances[instanceId].Intersect(&ray, t)
                        instanceId <- instanceId + 1
                else if ray.Direction[node.SplitAxis] > 0f then
                    Allocation.StackPush(traversalStack, &stackTop, node.RightChild)
                    Allocation.StackPush(traversalStack, &stackTop, i + 1)
                else
                    Allocation.StackPush(traversalStack, &stackTop, i + 1)
                    Allocation.StackPush(traversalStack, &stackTop, node.RightChild)

        hit

    override this.Intersect(ray, interaction, t) =
        let traversalStack = Allocation.StackAlloc<int>(BVHBuildConfig.MaxDepth + 1)
        let mutable stackTop = 0
        Allocation.StackPush(traversalStack, &stackTop, 0)
        let mutable hit = false

        while stackTop <> 0 do
            let i = Allocation.StackPop(traversalStack, &stackTop)
            let node = this.BVHNodes[i]

            if node.Bounds.Intersect(&ray, t) then
                if node.IsLeaf then
                    for instanceId = node.InstanceOffset to node.InstanceOffset + node.InstanceCount - 1 do
                        hit <- instances[instanceId].Intersect(&ray, &interaction, &t) || hit
                else if ray.Direction[node.SplitAxis] > 0f then
                    Allocation.StackPush(traversalStack, &stackTop, node.RightChild)
                    Allocation.StackPush(traversalStack, &stackTop, i + 1)
                else
                    Allocation.StackPush(traversalStack, &stackTop, i + 1)
                    Allocation.StackPush(traversalStack, &stackTop, node.RightChild)

        hit

    override this.Bounds = this.BVHNodes[0].Bounds
