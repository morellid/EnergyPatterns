// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FSCL
open InstructionEnergy
open TransferEnergy
open MetricBase
open Cloo
open Microsoft.FSharp.Collections
open EnergyPatterns.RemoteAmmeter
open MetricTools
open FSCL.KernelExtension
open FSCL.KernelFunctions
open FSCL.HostFunctions

// Example of macro
[<ReflectedDefinition>]
let filterWidth = 3

[<Kernel>][<ReflectedDefinition>]
let Convolution(input:float32[,], [<Constant>]filter:float32[,], output:float32[,], [<Local>]block:float32[,]) =
    let output_width = get_global_size(0)
    let input_width = output_width + filterWidth - 1
    let xOut = get_global_id(0)
    let yOut = get_global_id(1)

    let local_x = get_local_id(0)
    let local_y = get_local_id(1)
    let group_width = get_local_size(0)
    let group_height = get_local_size(1)
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
            sum <- sum + filter.[r,c] * block.[local_y + r, local_x + c]
    output.[yOut,xOut] <- sum
    
[<Kernel>][<ReflectedDefinition>]
let Reduce(g_idata:int[], [<Local>]sdata:int[], n, g_odata:int[]) =
    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    let tid = get_local_id(0)
    let i = get_group_id(0) * (get_local_size(0) * 2) + get_local_id(0)

    sdata.[tid] <- if(i < n) then g_idata.[i] else 0
    if (i + get_local_size(0) < n) then 
        sdata.[tid] <- sdata.[tid] + g_idata.[i + get_local_size(0)]

    barrier(CLK_LOCAL_MEM_FENCE)

    // do reduction in shared mem
    let mutable s = get_local_size(0) >>> 1
    while (s > 0) do 
        if (tid < s) then
            sdata.[tid] <- sdata.[tid] + sdata.[tid + s]
        barrier(CLK_LOCAL_MEM_FENCE)
        s <- s >>> 1

    if (tid = 0) then 
        g_odata.[get_group_id(0)] <- sdata.[0]
        
[<Kernel>][<ReflectedDefinition>]
let MatrixMult(a: float32[,], b: float32[,], c: float32[,]) =
    let x = get_global_id(0)
    let y = get_global_id(1)

    let mutable accum = 0.0f
    for k = 0 to a.GetLength(1) - 1 do
        accum <- accum + (a.[x,k] * b.[k,y])
    c.[x,y] <- accum
    
[<Kernel>][<ReflectedDefinition>]
let MatrixMultWithReturn(a: float32[,], b: float32[,]) =
    let c = Array2D.zeroCreate<float32> (a.GetLength(0)) (b.GetLength(1))

    let x = get_global_id(0)
    let y = get_global_id(1)

    let mutable accum = 0.0f
    for k = 0 to a.GetLength(1) - 1 do
        accum <- accum + (a.[x,k] * b.[k,y])
    c.[x,y] <- accum

    c
        
[<Kernel>][<ReflectedDefinition>]
let VectorAdd(a: float32[], b: float32[], c: float32[]) =
    let gid = get_global_id(0)
    c.[gid] <- (a.[gid] + b.[gid])
    
// Test functions
let testMatrixMultEnergy() =    
    // Create insturction energy metric
    let instructionMetric = InstructionEnergyMetric("131.114.88.115") 
    instructionMetric.DumpFolder <- Some("Dump")
    instructionMetric.MinInstr <- 1
    instructionMetric.MaxInstr <- 10000
    instructionMetric.InstrStep <- (fun i -> i * 2)
    instructionMetric.MinThread <- 1L
    instructionMetric.MaxThread <- (int64)(2 <<< 10)
    instructionMetric.ThreadStep <- (fun i -> i * 2L)
    instructionMetric.PerStepDuration <- 15000.0

    let compiler = new KernelCompiler(instructionMetric)
    let runner = new KernelRunner(compiler)
    compiler.Add(<@ MatrixMult @>) |> ignore
    
    let matA = Array2D.create 3 2 2.0f 
    let matB = Array2D.create 32 64 2.0f
    let matC = Array2D.zeroCreate<float32> 64 64
    let iterations = 1000
    let ev = instructionMetric.Evaluate([], <@ MatrixMult @>)
    let instr = instructionMetric.Instantiate([], ev, <@ MatrixMult(matA, matB, matC) @>, ([| matA.GetLength(0); matA.GetLength(1) |], [| 8; 8 |]))
    
    let endMsg, time, iterations = Tools.GetEnergyConsumption ("131.114.88.115") ((float)instructionMetric.PerStepDuration) (fun () ->
        runner.Run(<@ MatrixMult(matA, matB, matC) @>, [| matA.GetLength(0); matA.GetLength(1) |], [| 8; 8 |]))
    let avgen = System.Double.TryParse(endMsg.Replace(",", "."))

    let fileName = "MatrixMult_Real.csv"  
    let content = ref "Instructions,AvgEnergy,Duration,Iterations;\n"
    content := !content + instr.ToString() + "," + avgen.ToString() + "," + time.ToString() + "," + iterations.ToString() + ";\n"
    System.IO.File.WriteAllText(fileName, !content)
      
let testVectorAddEnergy() =    
    // Create insturction energy metric
    let instructionMetric = InstructionEnergyMetric("131.114.88.115") 
    instructionMetric.DumpFolder <- Some("Dump")
    instructionMetric.MinInstr <- 1
    instructionMetric.MaxInstr <- 10000
    instructionMetric.InstrStep <- (fun i -> i * 2)
    instructionMetric.MinThread <- 64L
    instructionMetric.MaxThread <- (int64)(2 <<< 10)
    instructionMetric.ThreadStep <- (fun i -> i * 2L)
    instructionMetric.PerStepDuration <- 10000.0

    let compiler = new KernelCompiler(instructionMetric)
    let runner = new KernelRunner(compiler)
    compiler.Add(<@ VectorAdd @>) |> ignore

    let a = Array.create (2 <<< 10) 2.0f 
    let b = Array.create (2 <<< 10) 2.0f
    let c = Array.zeroCreate<float32> (2 <<< 10)
    let ev = instructionMetric.Evaluate([], <@ VectorAdd @>)
    let instr = instructionMetric.Instantiate([], ev, <@ VectorAdd(a, b, c) @>, ([| a.Length |], [| 128 |]))
    
    let endMsg, time, iterations = Tools.GetEnergyConsumption ("131.114.88.115") ((float)instructionMetric.PerStepDuration) (fun () ->
        runner.Run(<@ VectorAdd(a, b, c) @>, [| a.Length |], [| 128 |]))

    let avgen = System.Double.TryParse(endMsg.Replace(",", "."))

    let fileName = "VectorAdd_Real.csv"  
    let content = ref "Instructions,AvgEnergy,Duration,Iterations;\n"
    content := !content + instr.ToString() + "," + avgen.ToString() + "," + time.ToString() + "," + iterations.ToString() + ";\n"
    System.IO.File.WriteAllText(fileName, !content)

[<EntryPoint>]
let main argv =
    let cd = new ComputationDensity.ComputationDensityMetric()
    cd.MaxThread <- 32L * 64L
    cd.DumpFolder <- Some("Dump")
    cd.InstrStep <- (fun i -> i * 2)
    cd.MaxInstr <- (16 <<< 10)
    cd.MinTransfer <- (512L <<< 10)
    cd.MaxTransfer <- (32L <<< 20)
    cd.TransferStep <- (fun i -> i * 2L)
    cd.PerStepDuration <- 2000.0
    let res = cd.Profile([ComputePlatform.Platforms.[0].Devices.[0]; ComputePlatform.Platforms.[0].Devices.[1]])

(*
    // Test 2 ways of getting instruction count    
    let instructionMetric = InstructionEnergyMetric("131.114.88.115") 
    let matA = Array2D.create 3 2 2.0f 
    let matB = Array2D.create 32 64 2.0f
    let matC = Array2D.zeroCreate<float32> 64 64
    let iterations = 1000
    let ev = instructionMetric.Evaluate([], <@ MatrixMult @>)
    //let ev2 = instructionMetric.Evaluate2([], <@ MatrixMult @>)
    let instr = instructionMetric.Instantiate([], ev, <@ MatrixMult(matA, matB, matC) @>, ([| matA.GetLength(0); matA.GetLength(1) |], [| 8; 8 |]))
    //let instr2 = instructionMetric.Instantiate([], ev2, <@ MatrixMult(matA, matB, matC) @>, ([| matA.GetLength(0); matA.GetLength(1) |], [| 8; 8 |]))
    *)
    let runner = new KernelRunner()
    // Test Vector Add
    (*
    let a = Array.create (2 <<< 10) 2.0f 
    let b = Array.create (2 <<< 10) 2.0f
    let c = Array.zeroCreate<float32> (2 <<< 10)
    // 2 ways, but only the first avoid copies of parameters
    runner.Run(<@ VectorAdd(a, b, c) @>, [| a.Length |], [| 128 |])
            
    // Test matrix multiplication
    let matA = Array2D.create 64 64 2.0f 
    let matB = Array2D.create 64 64 2.0f
    let matC = Array2D.zeroCreate<float32> 64 64
    runner.Run(<@ MatrixMult(matA, matB, matC) @>, [| matA.GetLength(0); matA.GetLength(1) |], [| 8; 8 |])
             *)
    // Test vector reduction 
    let a = Array.create (2 <<< 10) 2
    let b = Array.zeroCreate<int> (128)
    let c = Array.zeroCreate<int> (a.Length / 2 / 128)
    let mutable outputSize = c.Length
    runner.Run(<@ Reduce(a, b, a.Length, notused(c)) @>, [| a.Length / 2 |], [| 128 |])
    // Other reduction stages
    while outputSize > 2 do
        runner.Run(<@ Reduce(notused(c), b, outputSize, notused(c)) @>, [| outputSize |], [| System.Math.Min(outputSize, 64) |])
        outputSize <- outputSize / 2 / 128
    runner.Read(<@ c @>)
            
    // Test convolution
    let matA = Array2D.create 66 66 1.0f 
    let matF = Array2D.create filterWidth filterWidth 2.0f
    let matC = Array2D.zeroCreate<float32> (matA.GetLength(0) - filterWidth + 1) (matA.GetLength(1) - filterWidth + 1)
    let block = Array2D.zeroCreate<float32> (8 - filterWidth + 1) (8 - filterWidth + 1)
    runner.Run(<@ Convolution(matA, matF, matC, block) @>, [| matC.GetLength(0); matC.GetLength(1) |], [| 8; 8 |])

    0
