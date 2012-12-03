namespace FSCL

open fscl
open FSCL.Transformation

type Patterns() =
    [<Kernel>]
    [<ReflectedDefinition>]
    static member (*) (a: float32[,], b: float32[,]) =
        let c = Array2D.zeroCreate<float32> (a.Length) (b.Length)

        let x = get_global_id(0)
        let y = get_global_id(1)

        let mutable accum = 0.0f
        for k = 0 to a.GetLength(1) - 1 do
            accum <- accum + (a.[x,k] * b.[k,y])
        c.[x,y] <- accum

        c
    