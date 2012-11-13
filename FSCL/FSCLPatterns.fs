namespace FSCL

module Patterns =
    [<Kernel>]
    [<ReflectedDefinition>]
    let MatrixMult(a: float[,], b: float[,], c: float[,]) =
        for i = 0 to a.GetLength(0) - 1 do
            for j = 0 to b.GetLength(1) - 1 do
                let mutable accum = 0.0
                for k = 0 to a.GetLength(1) - 1 do
                     accum <- accum + (a.[i,k] * c.[k,j])
                c.[i,j] <- accum


