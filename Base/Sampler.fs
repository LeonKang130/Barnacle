namespace Barnacle.Base

open Barnacle.Util
open System.Numerics

[<Struct>]
type Sampler =
    val mutable state: uint
    new(x: uint) = { state = Hash.XXHash32Two(x, 0xdeadbeefu) }
    new(x: uint, y: uint) = { state = Hash.XXHash32Two(x, y) }
    new(x: uint, y: uint, z: uint) = { state = Hash.XXHash32Three(x, y, z) }
    member inline this.LoadState(state: uint) = this.state <- state
    member inline this.StoreState(state: uint byref) = state <- this.state
    member inline this.StoreState() = this.state
    member inline this.Next1D() = Hash.LCG &this.state
    member inline this.Next2D() = Vector2(this.Next1D(), this.Next1D())
