﻿namespace Barnacle.Util

open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<Struct; NoComparison; NoEquality>]
type Entry = {
    mutable alias: int
    mutable prob: float32
    mutable pdf: float32
}

type AliasTable(weights: float32 array) =
    member val Table =
        let sum = Array.sum weights
        if sum = 0f then
            Array.init weights.Length (fun i -> { prob = 1f; alias = i; pdf = 1f / float32 weights.Length })
        else
            let table = Array.mapi (fun i w -> { prob = w / sum; alias = i; pdf = weights[i] / sum }) weights
            let nextPowerOfTwo (v: int) =
                let mutable v = v - 1
                v <- v ||| (v >>> 1)
                v <- v ||| (v >>> 2)
                v <- v ||| (v >>> 4)
                v <- v ||| (v >>> 8)
                v <- v ||| (v >>> 16)
                v + 1
            let under = Stack<int>(nextPowerOfTwo weights.Length)
            let over = Stack<int>(nextPowerOfTwo weights.Length)
            for i = 0 to weights.Length - 1 do
                if table[i].prob > 1f then
                    over.Push i
                else
                    under.Push i
            while under.Count > 0 && over.Count > 0 do
                let o = over.Pop()
                let u = under.Pop()
                table[o].prob <- table[o].prob + table[u].prob - 1f
                table[u].alias <- o
                if table[o].prob > 1f then
                    over.Push o
                else
                    under.Push o
            for o in over do
                table[o].alias <- o
                table[o].prob <- 1f
            for u in under do
                table[u].alias <- u
                table[u].prob <- 1f
            table with get
    member inline this.Sample(u: float32) =
        let mutable table = &MemoryMarshal.GetArrayDataReference this.Table
        let mutable u = u * float32 this.Table.Length
        let idx = int u
        let entry = Unsafe.Add(&table, idx)
        u <- u - float32 idx
        if u < entry.prob then
            idx, entry.pdf
        else
            let idx = entry.alias
            idx, Unsafe.Add(&table, idx).pdf
