#nowarn "9"
namespace Barnacle.Base

open System
open System.Runtime.InteropServices

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
type private BVHBuildInstance =
    { bounds: AxisAlignedBoundingBox
      id: int }

[<Struct; NoComparison; NoEquality; StructLayout(LayoutKind.Explicit)>]
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

    static member inline CreateLeaf(bounds: AxisAlignedBoundingBox, instanceOffset: int, instanceCount: int) =
        let mutable node = Unchecked.defaultof<BVHNode>
        node.Bounds <- bounds
        node.InstanceOffset <- instanceOffset
        node.IsLeaf <- true
        node.InstanceCount <- instanceCount
        node

    static member inline CreateTemporary(bounds: AxisAlignedBoundingBox, splitAxis: int) =
        let mutable node = Unchecked.defaultof<BVHNode>
        node.Bounds <- bounds
        node.IsLeaf <- false
        node.SplitAxis <- splitAxis
        node

type private AABB = AxisAlignedBoundingBox

type private Tree =
    | Leaf of bounds: AABB * instanceOffset: int * instanceCount: int
    | Interior of bounds: AABB * splitAxis: int * leftChild: Tree * rightChild: Tree

[<Struct>]
type private Bin =
    val mutable Bounds: AABB
    val mutable Count: int
    new(bounds: AABB, count: int) = { Bounds = bounds; Count = count }
    static member inline Default = Bin(AABB.Default, 0)

    static member inline Union(a: Bin, b: Bin) =
        Bin(AABB.Union(a.Bounds, b.Bounds), a.Count + b.Count)

    member this.SAHCost = float32 this.Count * this.Bounds.SurfaceArea

type BVHBuilder(config: BVHBuildConfig) =
    member this.Config = config

    static member Default = BVHBuilder(BVHBuildConfig.Default)

    static member private Flatten(root: Tree) =
        let nodes = ResizeArray<BVHNode>()

        let rec flatten (tree: Tree) =
            let index = nodes.Count

            match tree with
            | Leaf(bounds, instanceOffset, instanceCount) ->
                nodes.Add(BVHNode.CreateLeaf(bounds, instanceOffset, instanceCount))
                index
            | Interior(bounds, splitAxis, leftChild, rightChild) ->
                let mutable node = BVHNode.CreateTemporary(bounds, splitAxis)
                nodes.Add(node)
                flatten leftChild |> ignore
                node.RightChild <- flatten rightChild
                nodes[index] <- node
                index
        flatten root |> ignore
        nodes.ToArray()

    member private this.BuildSubtree(instances: Span<BVHBuildInstance>, first: int, last: int, depth: int) =
        let subtreeInstanceCount = last - first
        let subtreeInstances = instances.Slice(first, subtreeInstanceCount).ToArray()

        let subtreeBounds =
            (AABB.Default, subtreeInstances)
            ||> Array.fold (fun aabb instance -> AABB.Union(aabb, instance.bounds))
        
        if subtreeInstanceCount <= this.Config.MaxLeafSize || depth >= this.Config.MaxDepth then
            Leaf(subtreeBounds, first, subtreeInstanceCount)
        else
            let centroidBounds =
                (AABB.Default, subtreeInstances)
                ||> Array.fold (fun aabb instance -> AABB.Union(aabb, instance.bounds.Center))

            let splitAxis = centroidBounds.SplitAxis
            let splitExtent = centroidBounds.Diagonal[splitAxis]

            if splitExtent = 0f then
                let mid = first + subtreeInstanceCount / 2
                let leftSubtree =
                    this.BuildSubtree(instances, first, mid, depth + 1)

                let rightSubtree =
                    this.BuildSubtree(instances, mid, last, depth + 1)

                Interior(subtreeBounds, splitAxis, leftSubtree, rightSubtree)
            else
                let mutable bins = Array.create config.SAHBinCount Bin.Default

                let inline findBinIndex (instance: BVHBuildInstance) =
                    ((float32 config.SAHBinCount
                      * (instance.bounds.Center[splitAxis] - centroidBounds.PMin[splitAxis]))
                     / splitExtent)
                    |> int
                    |> min (config.SAHBinCount - 1)

                for i = first to last - 1 do
                    let instance = subtreeInstances[i - first]
                    let binIndex = findBinIndex instance
                    bins[binIndex] <- Bin.Union(bins[binIndex], Bin(instance.bounds, 1))

                let costs = Array.zeroCreate<float32> (config.SAHBinCount - 1)
                let mutable leftBin = Bin.Default
                let mutable rightBin = Bin.Default

                for i = 0 to config.SAHBinCount - 2 do
                    leftBin <- Bin.Union(leftBin, bins[i])
                    rightBin <- Bin.Union(rightBin, bins[config.SAHBinCount - 1 - i])
                    costs[i] <- costs[i] + leftBin.SAHCost
                    costs[config.SAHBinCount - 2 - i] <- costs[config.SAHBinCount - 2 - i] + rightBin.SAHCost

                let splitIndex =
                    costs |> Array.mapi (fun i cost -> i, cost) |> Array.minBy snd |> fst

                let leftSubtreeInstances, rightSubtreeInstances =
                    subtreeInstances
                    |> Array.partition (fun instance -> findBinIndex instance <= splitIndex)

                leftSubtreeInstances.CopyTo(instances.Slice(first, leftSubtreeInstances.Length))

                rightSubtreeInstances.CopyTo(
                    instances.Slice(first + leftSubtreeInstances.Length, rightSubtreeInstances.Length)
                )

                let leftSubtree =
                    this.BuildSubtree(instances, first, first + leftSubtreeInstances.Length, depth + 1)

                let rightSubtree =
                    this.BuildSubtree(instances, first + leftSubtreeInstances.Length, last, depth + 1)

                Interior(subtreeBounds, splitAxis, leftSubtree, rightSubtree)

    member this.Build(aabbs: AABB array): int array * BVHNode array =
        let instances = aabbs |> Array.mapi (fun i aabb -> { bounds = aabb; id = i })
        let nodes = this.BuildSubtree(instances.AsSpan(), 0, instances.Length, 0) |> BVHBuilder.Flatten
        let indices = instances |> Array.map (_.id)
        indices, nodes
