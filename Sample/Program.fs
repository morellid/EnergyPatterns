// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FSCL
open InstructionEnergy
open TransferEnergy
open MetricBase
open Cloo
open Microsoft.FSharp.Collections

[<ReflectedDefinition>]
let Convolution(input:float32[,], [<Constant>]filter:float32[,], output:float32[,], [<Local>] block:float32[,], filterWidth:int) =
    let output_width = fscl.get_global_size(0)
    let input_width = output_width + filterWidth - 1
    let xOut = fscl.get_global_id(0)
    let yOut = fscl.get_global_id(1)

    let local_x = fscl.get_local_id(0)
    let local_y = fscl.get_local_id(1)
    let group_width = fscl.get_local_size(0)
    let group_height = fscl.get_local_size(1)
    let block_width = group_width + filterWidth - 1
    let block_height = group_height + filterWidth - 1

    //Set required rows into the LDS
    let mutable global_start_x = xOut
    let mutable global_start_y = yOut
    for local_start_x in local_x .. group_width .. block_width do
        for local_start_y in local_y .. group_height .. block_height do    
            block.[local_start_y, local_start_x] <- input.[global_start_y, global_start_x]
            global_start_x <- global_start_x + group_width
            global_start_y <- global_start_y + group_height
        
    let mutable sum = 0.0f
    for r = 0 to filterWidth - 1 do
        for c = 0 to filterWidth - 1 do
            sum <- filter.[r,c] * block.[local_y + r, local_x + c]

    output.[yOut,xOut] <- sum

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

    // Dump instruction energy profiling
    let instructionMetric = InstructionEnergyMetric("131.114.88.115") 
    instructionMetric.DumpFolder <- Some("Dump")
    instructionMetric.MinInstr <- 1
    instructionMetric.MaxInstr <- 2048
    instructionMetric.Step <- 100
    instructionMetric.PerStepDuration <- 20000
    instructionMetric.ThreadCount <- 2048L
    for device in ComputePlatform.Platforms.[0].Devices do
        instructionMetric.Profile(device) |> ignore
        
    // Dump memory transfer energy profiling
    let transferMetric = TransferEnergyMetric("131.114.88.115") 
    transferMetric.DumpFolder <- Some("Dump")
    transferMetric.MinSize <- (1 <<< 10)
    transferMetric.MaxSize <- (32 <<< 20)
    transferMetric.Step <- (1 <<< 20)
    transferMetric.PerStepDuration <- 20000
    transferMetric.SrcInfo <- TransferEnergy.Data.TransferEndpoint()
    transferMetric.DstInfo <- TransferEnergy.Data.TransferEndpoint()
    transferMetric.SrcInfo.IsHostPtr <- true
    transferMetric.DstInfo.IsHostPtr <- false
    for device in ComputePlatform.Platforms.[0].Devices do
        transferMetric.Profile(device) |> ignore
        
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
