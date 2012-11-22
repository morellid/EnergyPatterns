// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FSCL
open InstructionEnergy
open MetricBase
open Cloo
open Microsoft.FSharp.Collections

[<Kernel>]
[<ReflectedDefinition>]
let MatrixMult(a: float32[,], b: float32[,], c: float32[,]) =
    let x = fscl.get_global_id(0)
    let y = fscl.get_global_id(1)
    
    let mutable accum = 0.0f
    for k = 0 to a.GetLength(1) - 1 do
        accum <- accum + (a.[x,k] * b.[k,y])
    c.[x,y] <- accum
    
[<Kernel>]
[<ReflectedDefinition>]
let VectorAdd(a: float32[], b: float32[], c: float32[]) =
    let gid = fscl.get_global_id(0)
    c.[gid] <- (a.[gid] + b.[gid])

[<EntryPoint>]
let main argv =
    let runner = new KernelRunner()
(*
    let metric = new InstructionEnergyMetric()
    metric.MinInstr <- 1
    metric.MaxInstr <- 2048
    metric.Step <- 100
    metric.PerStepDuration <- 20000
    metric.ThreadCount <- 2048L
    *)
    // Test vector addition
    let a = Array.create 10 10.0f
    let b = Array.create 10 10.0f
    let c = Array.zeroCreate<float32> 10
    runner.Run(<@ VectorAdd(a, b, c) @>, 
               [| (10L, 10L) |])

    // Test matrix multiplication
    let matA = Array2D.create 64 64 2.0f 
    let matB = Array2D.create 64 64 2.0f
    let matC = Array2D.zeroCreate<float32> 64 64
    runner.Run(<@ MatrixMult(matA, matB, matC) @>, 
               [| (matA.GetLongLength(0), 8L); (matA.GetLongLength(1), 8L) |])
    
    // Test prettyPrinting
    //let (str, a) = (FSCL.KernelBinding.ConvertToCLKernel(<@ MatrixMult @>)).Value
    //printf "%s" str
    (*
    let b = Array.create 10 10.0
    let c = Array.create 10 10.0
    let count = 9

    let m = metric :> AbsoluteMetric<ComputeDevice, EnergyProfilingResult, EnergyInstantiationResult>
    let energyLambda = m.Evaluate(new ProfilingResult<EnergyProfilingResult>([]), <@ VectorAdd @>)
    let instructions = m.Instantiate(energyLambda, <@ VectorAdd((Array.create 10 10.0), b, c, 2.0) @>)
    printf "Number of instruction in the kernel (args = a, b, c): %f\n" instructions.Result
    *)
    // Matrix mult
    (*
    let matA = array2D [ [ 1.0; 2.0 ]; 
                         [ 1.0; 2.0 ] ]
    let matB = array2D [ [ 1.0; 2.0 ]; 
                         [ 1.0; 2.0 ] ]
    let matC = Array2D.create 2 2 0.0
    
    let m2 = metric :> AbsoluteMetric<ComputeDevice, EnergyProfilingResult, EnergyInstantiationResult>
    // FICHISSIMO: DLL espongono i corpi delle funzioni ReflectedDefinition (libreria di pattern plausibile)
    let energyLambda2 = m2.Evaluate(new ProfilingResult<EnergyProfilingResult>([]), <@ Patterns.MatrixMult(matA, matB, matC) @>)
    let instructions2 = m2.Instantiate(energyLambda2, <@ MatrixMult(matA, matB, matC) @>)
    printf "Number of instruction in the kernel (args = a, b, c): %f\n" instructions2.Result
    // Testing vector types
    (*
    let t = new FSCL.OpenCLVector4D()
    t.s0
    t.xw <- [| 0.; 1. |]
    t.s1230 <- [| 1.;2.;3.;4. |]
    let g = t.even
    let g2 = t.odd
    printfn "%A" argv
    *)
    0 // return an integer exit code *)
    0
