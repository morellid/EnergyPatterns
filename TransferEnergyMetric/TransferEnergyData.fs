namespace TransferEnergy.Data

open Cloo

type TransferEndpoint() = 
    let isHostPtr = true
    let flags = ComputeMemoryFlags.None
    let shouldMap = false

    member val IsHostPtr = isHostPtr with get, set
    member val Flags = flags with get, set
    member val ShouldMap = shouldMap with get, set

