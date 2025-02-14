namespace Barnacle.Extensions.Aggregate

open Barnacle.Base

type BVHAggregate(instances: PrimitiveInstance array) =
    inherit PrimitiveAggregate()
    let indices, nodes =
        instances
        |> Array.map (_.Bounds)
        |> BVHBuilder.Default.Build
    let instances =
        Array.init instances.Length (fun i -> instances[indices[i]])
    member private this.TraverseAny(ray: Ray inref, t: float32, i: int) =
        let node = nodes[i]
        if node.Bounds.Intersect(&ray, t) then
            if node.IsLeaf then
                let mutable hit = false
                let mutable instanceId = node.InstanceOffset
                while not hit && instanceId < node.InstanceOffset + node.InstanceCount do
                    hit <- instances[instanceId].Intersect(&ray, t)
                    instanceId <- instanceId + 1
                hit
            else
                if ray.Direction[node.SplitAxis] > 0f then
                    this.TraverseAny(&ray, t, i + 1) || this.TraverseAny(&ray, t, node.RightChild)
                else
                    this.TraverseAny(&ray, t, node.RightChild) || this.TraverseAny(&ray, t, i + 1)
        else
            false
    member private this.TraverseClosest(ray: Ray inref, interaction: Interaction outref, t: float32 byref, i: int) =
        let node = nodes[i]
        if node.Bounds.Intersect(&ray, t) then
            if node.IsLeaf then
                let mutable hit = false
                for instanceId = node.InstanceOffset to node.InstanceOffset + node.InstanceCount - 1 do
                    hit <- instances[instanceId].Intersect(&ray, &interaction, &t) || hit
                hit
            else
                if ray.Direction[node.SplitAxis] > 0f then
                    let hit = this.TraverseClosest(&ray, &interaction, &t, i + 1)
                    this.TraverseClosest(&ray, &interaction, &t, node.RightChild) || hit
                else
                    let hit = this.TraverseClosest(&ray, &interaction, &t, node.RightChild)
                    this.TraverseClosest(&ray, &interaction, &t, i + 1) || hit
        else
            false
    override this.Intersect(ray: Ray inref, t: float32) =
        this.TraverseAny(&ray, t, 0)
    override this.Intersect(ray: Ray inref, interaction: Interaction outref, t: float32 byref) =
        this.TraverseClosest(&ray, &interaction, &t, 0)
