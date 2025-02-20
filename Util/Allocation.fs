#nowarn "9"
namespace Barnacle.Util

open System
open System.Runtime.CompilerServices
open Microsoft.FSharp.NativeInterop

module Allocation =
    let inline StackAlloc<'a when 'a: unmanaged> (length: int): 'a nativeptr =
        NativePtr.stackalloc<'a> length
    
    let inline StackAllocAsSpan<'a when 'a: unmanaged> (length: int): 'a Span =
        let p = NativePtr.stackalloc<'a> length |> NativePtr.toVoidPtr
        Span(p, length)
    
    let inline StackPush<'a when 'a: unmanaged> (stack: 'a nativeptr, stackTop: int byref, value: 'a) =
        Unsafe.Add((NativePtr.toByRef stack), stackTop) <- value
        stackTop <- stackTop + 1
    
    let inline StackPop<'a when 'a: unmanaged> (stack: 'a nativeptr, stackTop: int byref) : 'a byref =
        stackTop <- stackTop - 1
        &Unsafe.Add((NativePtr.toByRef stack), stackTop)
    
    let inline StackRead<'a when 'a: unmanaged> (stack: 'a nativeptr, index: int) : 'a byref =
        &Unsafe.Add((NativePtr.toByRef stack), index)
