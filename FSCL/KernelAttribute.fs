namespace FSCL

open System

type KernelAttribute =
    inherit Attribute

    val dev : (int * int) option

    new(device: (int * int)) =  { 
        dev = Some(device) 
    }
    new() =  { 
        dev = None 
    }

    member this.Platform 
        with get() = if(this.dev.IsSome) then fst(this.dev.Value) else -1
    member this.Device
        with get() = if(this.dev.IsSome) then snd(this.dev.Value) else -1


