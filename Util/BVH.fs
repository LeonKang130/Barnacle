#nowarn "9"
namespace Barnacle.Util

open System
open System.Numerics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<IsReadOnly; Struct>]
type AxisAlignedBoundingBox =
    val pMin: Vector3
    val pMax: Vector3
    new(pMin: Vector3, pMax: Vector3) = { pMin = pMin; pMax = pMax }
    member inline this.Centroid = 0.5f * (this.pMin + this.pMax)
    member inline this.Diagonal = this.pMax - this.pMin

    member inline this.SurfaceArea =
        let diag = this.Diagonal
        2f * (diag.X * diag.Y + diag.Y * diag.Z + diag.Z * diag.X)

    member inline this.Volume =
        let diag = this.Diagonal
        diag.X * diag.Y * diag.Z
    member inline this.SplitAxis =
        let diag = this.Diagonal
        if diag.X >= diag.Y && diag.X >= diag.Z then 0
        elif diag.Y >= diag.Z then 1 else 2

    static member inline Transform(aabb: AxisAlignedBoundingBox inref, m: Matrix4x4) =
        let mutable pMin, pMax = Vector3.PositiveInfinity, Vector3.NegativeInfinity

        for i = 0 to 7 do
            let pX = if i &&& 1 = 0 then aabb.pMin.X else aabb.pMax.X
            let pY = if i &&& 2 = 0 then aabb.pMin.Y else aabb.pMax.Y
            let pZ = if i &&& 4 = 0 then aabb.pMin.Z else aabb.pMax.Z
            let p = Vector3.Transform(Vector3(pX, pY, pZ), m)
            pMin <- Vector3.MinNative(pMin, p)
            pMax <- Vector3.MaxNative(pMax, p)

        AxisAlignedBoundingBox(pMin, pMax)
    static member inline Transform(aabb: AxisAlignedBoundingBox, m: Matrix4x4) =
        AxisAlignedBoundingBox.Transform(&aabb, m)
    
    static member inline Union(a: AxisAlignedBoundingBox, b: AxisAlignedBoundingBox) =
        AxisAlignedBoundingBox(Vector3.MinNative(a.pMin, b.pMin), Vector3.MaxNative(a.pMax, b.pMax))

    static member inline Union(a: AxisAlignedBoundingBox, b: Vector3) =
        AxisAlignedBoundingBox(Vector3.MinNative(a.pMin, b), Vector3.MaxNative(a.pMax, b))

    static member Default = AxisAlignedBoundingBox(Vector3.PositiveInfinity, Vector3.NegativeInfinity)

[<Struct; NoComparison; NoEquality; StructLayout(LayoutKind.Explicit, Size = 32)>]
type BVHNode =
    [<FieldOffset 0>]
    val mutable Bounds: AxisAlignedBoundingBox

    [<FieldOffset 24>]
    val mutable RightChild: int

    [<FieldOffset 24>]
    val mutable InstanceOffset: int

    [<FieldOffset 28>]
    val mutable IsLeaf: bool

    [<FieldOffset 29>]
    val mutable internal splitAxis: int8

    [<FieldOffset 30>]
    val mutable internal instanceCount: int8

    [<FieldOffset 31>]
    val mutable VisibilityMask: uint8 // for future use

    member this.SplitAxis
        with get () = int this.splitAxis
        and set (v: int) = this.splitAxis <- int8 v

    member this.InstanceCount
        with get () = int this.instanceCount
        and set (v: int) = this.instanceCount <- int8 v
    
    static member inline CreateLeaf(bounds: AxisAlignedBoundingBox, instanceOffset: int, instanceCount: int) =
        let mutable node = Unchecked.defaultof<BVHNode>
        node.Bounds <- bounds
        node.InstanceOffset <- instanceOffset
        node.IsLeaf <- true
        node.InstanceCount <- instanceCount
        node
    
    static member inline CreateInterior(bounds: AxisAlignedBoundingBox, rightChild: int, splitAxis: int) =
        let mutable node = Unchecked.defaultof<BVHNode>
        node.Bounds <- bounds
        node.RightChild <- rightChild
        node.IsLeaf <- false
        node.SplitAxis <- splitAxis
        node

module BVHBuildConfig =
    [<Literal>]
    let SAHBinCount = 12

    [<Literal>]
    let MaxLeafSize = 4

    [<Literal>]
    let MaxDepth = 64

module private BVHBuildUtil =

    type AABB = AxisAlignedBoundingBox

    [<Struct>]
    type Bin =
        val mutable Bounds: AABB
        val mutable Count: int
        new(bounds: AABB, count: int) = { Bounds = bounds; Count = count }
        static member inline Default = Bin(AABB.Default, 0)

        static member inline Union(a: Bin, b: Bin) =
            Bin(AABB.Union(a.Bounds, b.Bounds), a.Count + b.Count)

        member inline this.SAHCost = float32 this.Count * this.Bounds.SurfaceArea

    type Tree =
        | Leaf of bounds: AABB * instanceOffset: int * instanceCount: int
        | Interior of bounds: AABB * splitAxis: int * leftChild: Tree * rightChild: Tree
        static member Build(instances: struct (AABB * int) Span, first: int, last: int, depth: int) =
            let subtreeInstanceCount = last - first
            let subtreeInstances = instances.Slice(first, subtreeInstanceCount).ToArray()

            let mutable subtreeBounds = AABB.Default

            for i = 0 to subtreeInstanceCount - 1 do
                let struct (bounds, _) = subtreeInstances[i]
                subtreeBounds <- AABB.Union(subtreeBounds, bounds)

            if subtreeInstanceCount <= BVHBuildConfig.MaxLeafSize || depth >= BVHBuildConfig.MaxDepth then
                Leaf(subtreeBounds, first, subtreeInstanceCount)
            else
                let mutable centroidBounds = AABB.Default

                for i = 0 to subtreeInstanceCount - 1 do
                    let struct (bounds, _) = subtreeInstances[i]
                    centroidBounds <- AABB.Union(centroidBounds, bounds.Centroid)

                let splitAxis = centroidBounds.SplitAxis
                let splitExtent = centroidBounds.Diagonal[splitAxis]

                if splitExtent = 0f then
                    let mid = first + subtreeInstanceCount / 2
                    let leftSubtree = Tree.Build(instances, first, mid, depth + 1)

                    let rightSubtree = Tree.Build(instances, mid, last, depth + 1)

                    Interior(subtreeBounds, splitAxis, leftSubtree, rightSubtree)
                else
                    let bins = Allocation.StackAllocAsSpan<Bin> BVHBuildConfig.SAHBinCount

                    for i = 0 to BVHBuildConfig.SAHBinCount - 1 do
                        bins[i] <- Bin.Default

                    let inline findBinIndex (instance: struct (AABB * int)) =
                        let struct (bounds, _) = instance
                        ((float32 BVHBuildConfig.SAHBinCount
                          * (bounds.Centroid[splitAxis] - centroidBounds.pMin[splitAxis]))
                         / splitExtent)
                        |> int
                        |> min (BVHBuildConfig.SAHBinCount - 1)

                    for i = first to last - 1 do
                        let instance = subtreeInstances[i - first]
                        let binIndex = findBinIndex instance
                        let bin = &bins[binIndex]
                        let struct (bounds, _) = instance
                        bin.Bounds <- AABB.Union(bin.Bounds, bounds)
                        bin.Count <- bin.Count + 1

                    let costs = Allocation.StackAllocAsSpan<float32> (BVHBuildConfig.SAHBinCount - 1)

                    for i = 0 to costs.Length - 1 do
                        costs[i] <- 0f

                    let mutable leftBin = Bin.Default
                    let mutable rightBin = Bin.Default

                    for i = 0 to costs.Length - 1 do
                        leftBin.Bounds <- AABB.Union(leftBin.Bounds, bins[i].Bounds)
                        leftBin.Count <- leftBin.Count + bins[i].Count
                        costs[i] <- costs[i] + leftBin.SAHCost
                        rightBin.Bounds <- AABB.Union(rightBin.Bounds, bins[costs.Length - i].Bounds)
                        rightBin.Count <- rightBin.Count + bins[costs.Length - i].Count
                        costs[costs.Length - 1 - i] <- costs[costs.Length - 1 - i] + rightBin.SAHCost

                    let mutable minCost = infinityf
                    let mutable bestSplitIndex = 0

                    for i = 0 to costs.Length - 1 do
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

                    let leftSubtree = Tree.Build(instances, first, left, depth + 1)

                    let rightSubtree = Tree.Build(instances, right + 1, last, depth + 1)

                    Interior(subtreeBounds, splitAxis, leftSubtree, rightSubtree)
        member inline this.Flatten() =
            let nodes = ResizeArray<BVHNode>()
            let rec flatten (tree: Tree) =
                let index = nodes.Count
                match tree with
                | Leaf(bounds, instanceOffset, instanceCount) ->
                    nodes.Add(BVHNode.CreateLeaf(bounds, instanceOffset, instanceCount))
                | Interior(bounds, splitAxis, leftChild, rightChild) ->
                    nodes.Add(Unchecked.defaultof<BVHNode>)
                    flatten leftChild |> ignore
                    nodes[index] <- BVHNode.CreateInterior(bounds, flatten rightChild, splitAxis)
                index
            flatten this |> ignore
            nodes.ToArray()

type BVHNode with
    static member Build(xs: 'a Span, f: 'a -> AxisAlignedBoundingBox) =
        let xs' = xs.ToArray()
        let instances = xs' |> Array.mapi (fun i x -> struct ((f x), i))
        let nodes = BVHBuildUtil.Tree.Build(instances.AsSpan(), 0, instances.Length, 0).Flatten()
        for i = 0 to xs.Length - 1 do
            let struct (_, index) = instances[i]
            xs[i] <- xs'[index]
        nodes
