#nowarn "9"
namespace Barnacle.Base

open System
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop

[<Struct; NoComparison; NoEquality; StructLayout(LayoutKind.Explicit, Size = 32)>]
type BVHNode =
    [<DefaultValue; FieldOffset 0>]
    val mutable Bounds: AxisAlignedBoundingBox

    [<DefaultValue; FieldOffset 24>]
    val mutable RightChild: int

    [<DefaultValue; FieldOffset 24>]
    val mutable InstanceOffset: int

    [<DefaultValue; FieldOffset 28>]
    val mutable IsLeaf: bool

    [<DefaultValue; FieldOffset 29>]
    val mutable internal splitAxis: int8

    [<DefaultValue; FieldOffset 30>]
    val mutable internal instanceCount: int8

    [<DefaultValue; FieldOffset 31>]
    val mutable VisibilityMask: uint8 // for future use

    member this.SplitAxis
        with get () = int this.splitAxis
        and set (v: int) = this.splitAxis <- int8 v

    member this.InstanceCount
        with get () = int this.instanceCount
        and set (v: int) = this.instanceCount <- int8 v

module BVHBuildUtil =
    [<Struct>]
    type BVHBuildConfig =
        { MaxLeafSize: int
          MaxDepth: int
          SAHBinCount: int }

        static member Default =
            { MaxLeafSize = 4
              MaxDepth = 64
              SAHBinCount = 16 }

    [<Struct>]
    type BVHBuildInstance =
        { bounds: AxisAlignedBoundingBox
          id: int }

    type AABB = AxisAlignedBoundingBox

    type Tree =
        | Leaf of bounds: AABB * instanceOffset: int * instanceCount: int
        | Interior of bounds: AABB * splitAxis: int * leftChild: Tree * rightChild: Tree

    [<Struct>]
    type Bin =
        val mutable Bounds: AABB
        val mutable Count: int
        new(bounds: AABB, count: int) = { Bounds = bounds; Count = count }
        static member inline Default = Bin(AABB.Default, 0)

        static member inline Union(a: Bin, b: Bin) =
            Bin(AABB.Union(a.Bounds, b.Bounds), a.Count + b.Count)

        member inline this.SAHCost = float32 this.Count * this.Bounds.SurfaceArea


    let inline stackalloc<'a when 'a: unmanaged> (length: int) : Span<'a> =
        let p = NativePtr.stackalloc<'a> length |> NativePtr.toVoidPtr
        Span<'a>(p, length)

    let inline createLeaf (bounds: AABB inref, instanceOffset: int, instanceCount: int) =
        let mutable node = Unchecked.defaultof<BVHNode>
        node.Bounds <- bounds
        node.InstanceOffset <- instanceOffset
        node.IsLeaf <- true
        node.InstanceCount <- instanceCount
        node

    let inline createInterior (bounds: AABB inref, rightChild: int, splitAxis: int) =
        let mutable node = Unchecked.defaultof<BVHNode>
        node.Bounds <- bounds
        node.RightChild <- rightChild
        node.IsLeaf <- false
        node.SplitAxis <- splitAxis
        node

    type BVHBuilder(config: BVHBuildConfig) =
        member this.Config = config

        static member Default = BVHBuilder(BVHBuildConfig.Default)

        static member private Flatten(root: Tree) =
            let nodes = ResizeArray<BVHNode>()

            let rec flatten (tree: Tree) =
                let index = nodes.Count

                match tree with
                | Leaf(bounds, instanceOffset, instanceCount) ->
                    nodes.Add(createLeaf (&bounds, instanceOffset, instanceCount))
                    index
                | Interior(bounds, splitAxis, leftChild, rightChild) ->
                    nodes.Add(Unchecked.defaultof<BVHNode>)
                    flatten leftChild |> ignore
                    nodes[index] <- createInterior (&bounds, flatten rightChild, splitAxis)
                    index

            flatten root |> ignore
            nodes.ToArray()

        member private this.BuildSubtree(instances: Span<BVHBuildInstance>, first: int, last: int, depth: int) =
            let subtreeInstanceCount = last - first
            let subtreeInstances = instances.Slice(first, subtreeInstanceCount).ToArray()

            let mutable subtreeBounds = AABB.Default

            for i = 0 to subtreeInstanceCount - 1 do
                let bounds = &subtreeInstances[i].bounds
                subtreeBounds <- AABB.Union(subtreeBounds, bounds)

            if subtreeInstanceCount <= this.Config.MaxLeafSize || depth >= this.Config.MaxDepth then
                Leaf(subtreeBounds, first, subtreeInstanceCount)
            else
                let mutable centroidBounds = AABB.Default

                for i = 0 to subtreeInstanceCount - 1 do
                    let centroid = subtreeInstances[i].bounds.Center
                    centroidBounds <- AABB.Union(centroidBounds, centroid)

                let splitAxis = centroidBounds.SplitAxis
                let splitExtent = centroidBounds.Diagonal[splitAxis]

                if splitExtent = 0f then
                    let mid = first + subtreeInstanceCount / 2
                    let leftSubtree = this.BuildSubtree(instances, first, mid, depth + 1)

                    let rightSubtree = this.BuildSubtree(instances, mid, last, depth + 1)

                    Interior(subtreeBounds, splitAxis, leftSubtree, rightSubtree)
                else
                    let bins = stackalloc<Bin> config.SAHBinCount

                    for i = 0 to config.SAHBinCount - 1 do
                        bins[i] <- Bin.Default

                    let inline findBinIndex (instance: BVHBuildInstance) =
                        ((float32 config.SAHBinCount
                          * (instance.bounds.Center[splitAxis] - centroidBounds.pMin[splitAxis]))
                         / splitExtent)
                        |> int
                        |> min (config.SAHBinCount - 1)

                    for i = first to last - 1 do
                        let instance = subtreeInstances[i - first]
                        let binIndex = findBinIndex instance
                        let bin = &bins[binIndex]
                        bin.Bounds <- AABB.Union(bin.Bounds, instance.bounds)
                        bin.Count <- bin.Count + 1

                    let costs = stackalloc<float32> (config.SAHBinCount - 1)

                    for i = 0 to config.SAHBinCount - 2 do
                        costs[i] <- 0f

                    let mutable leftBin = Bin.Default
                    let mutable rightBin = Bin.Default

                    for i = 0 to config.SAHBinCount - 2 do
                        leftBin.Bounds <- AABB.Union(leftBin.Bounds, bins[i].Bounds)
                        leftBin.Count <- leftBin.Count + bins[i].Count
                        costs[i] <- costs[i] + leftBin.SAHCost
                        rightBin.Bounds <- AABB.Union(rightBin.Bounds, bins[config.SAHBinCount - 1 - i].Bounds)
                        rightBin.Count <- rightBin.Count + bins[config.SAHBinCount - 1 - i].Count
                        costs[config.SAHBinCount - 2 - i] <- costs[config.SAHBinCount - 2 - i] + rightBin.SAHCost

                    let mutable minCost = infinityf
                    let mutable bestSplitIndex = 0

                    for i = 0 to config.SAHBinCount - 2 do
                        let cost = costs[i] + float32 subtreeInstanceCount * centroidBounds.SurfaceArea

                        if cost < minCost then
                            minCost <- cost
                            bestSplitIndex <- i

                    let mutable left = first
                    let mutable right = last - 1

                    for i = 0 to subtreeInstanceCount - 1 do
                        let instance = subtreeInstances[i]
                        let binIndex = findBinIndex instance

                        if binIndex > bestSplitIndex then
                            instances[right] <- instance
                            right <- right - 1
                        else
                            instances[left] <- instance
                            left <- left + 1

                    let leftSubtree = this.BuildSubtree(instances, first, left, depth + 1)

                    let rightSubtree = this.BuildSubtree(instances, right + 1, last, depth + 1)

                    Interior(subtreeBounds, splitAxis, leftSubtree, rightSubtree)

        member this.Build(items: Span<'T>, getBounds: 'T -> AABB) : BVHNode array =
            let temp = items.ToArray()
            let instances = Array.zeroCreate<BVHBuildInstance> items.Length

            for i = 0 to items.Length - 1 do
                instances[i] <- { bounds = getBounds temp[i]; id = i }

            let nodes =
                this.BuildSubtree(instances.AsSpan(), 0, instances.Length, 0)
                |> BVHBuilder.Flatten

            for i = 0 to items.Length - 1 do
                items[i] <- temp[instances[i].id]

            nodes

type BVHBuilder = BVHBuildUtil.BVHBuilder
