// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FSCL
open InstructionEnergy
open TransferEnergy
open MetricBase
open Cloo
open Microsoft.FSharp.Collections
open fscl
open EnergyPatterns.RemoteAmmeter

// Example of macro
[<ReflectedDefinition>]
let filterWidth = 3

[<ReflectedDefinition>]
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
            sum <- filter.[r,c] * block.[local_y + r, local_x + c]
    output.[yOut,xOut] <- sum

[<Kernel>]
[<ReflectedDefinition>]
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
        
[<Kernel>]
[<ReflectedDefinition>]
let MatrixMult(a: float32[,], b: float32[,], c: float32[,]) =
    let x = get_global_id(0)
    let y = get_global_id(1)

    let mutable accum = 0.0f
    for k = 0 to a.GetLength(1) - 1 do
        accum <- accum + (a.[x,k] * b.[k,y])
    c.[x,y] <- accum
    
[<Kernel>]
[<ReflectedDefinition>]
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
    instructionMetric.Step <- 1000
    instructionMetric.PerStepDuration <- 15000
    instructionMetric.ThreadCount <- 2048L

    let compiler = new KernelCompiler(instructionMetric)
    let runner = new KernelRunner(compiler)
    
    let matA = Array2D.create 64 32 2.0f 
    let matB = Array2D.create 32 64 2.0f
    let matC = Array2D.zeroCreate<float32> 64 64
    let energyClient = new Client("131.114.88.115")
    let iterations = 1000
    let ev = instructionMetric.Evaluate([], <@ MatrixMult @>)
    let instr = instructionMetric.Instantiate([], ev, <@ MatrixMult(matA, matB, matC) @>, ([| matA.GetLength(0); matA.GetLength(1) |], [| 8; 8 |]))
    
    let timer = System.Diagnostics.Stopwatch()
    let startMsg = energyClient.start()
    timer.Start()
    for i in 0 .. iterations - 1 do
        runner.Run(<@ MatrixMult(matA, matB, matC) @>, 
                   [| matA.GetLength(0); matA.GetLength(1) |], [| 8; 8 |])
    timer.Stop()
    let endMsg = energyClient.stop()
    let avgen = System.Double.TryParse(endMsg.Replace(",", "."))

    let fileName = "MatrixMult_Real.csv"  
    let content = ref "Instructions,AvgEnergy,Duration,Iterations;\n"
    content := !content + instr.ToString() + "," + avgen.ToString() + "," + timer.ElapsedMilliseconds.ToString() + "," + iterations.ToString() + ";\n"
    System.IO.File.WriteAllText(fileName, !content)
    
let testVectorAddEnergy() =    
    // Create insturction energy metric
    let instructionMetric = InstructionEnergyMetric("131.114.88.115") 
    instructionMetric.DumpFolder <- Some("Dump")
    instructionMetric.MinInstr <- 1
    instructionMetric.MaxInstr <- 10000
    instructionMetric.Step <- 1000
    instructionMetric.PerStepDuration <- 15000
    instructionMetric.ThreadCount <- 2048L

    let compiler = new KernelCompiler(instructionMetric)
    let runner = new KernelRunner(compiler)
    
    let a = Array.create (2 <<< 10) 2.0f 
    let b = Array.create (2 <<< 10) 2.0f
    let c = Array.zeroCreate<float32> (2 <<< 10)
    let energyClient = new Client("131.114.88.115")
    let iterations = 1000
    let ev = instructionMetric.Evaluate([], <@ VectorAdd @>)
    let instr = instructionMetric.Instantiate([], ev, <@ VectorAdd(a, b, c) @>, ([| a.Length |], [| 128 |]))
    
    let timer = System.Diagnostics.Stopwatch()
    let startMsg = energyClient.start()
    timer.Start()
    for i in 0 .. iterations - 1 do
        runner.Run(<@ VectorAdd(a, b, c) @>, [| a.Length |], [| 128 |])
    timer.Stop()
    let endMsg = energyClient.stop()
    let avgen = System.Double.TryParse(endMsg.Replace(",", "."))

    let fileName = "VectorAdd_Real.csv"  
    let content = ref "Instructions,AvgEnergy,Duration,Iterations;\n"
    content := !content + instr.ToString() + "," + avgen.ToString() + "," + timer.ElapsedMilliseconds.ToString() + "," + iterations.ToString() + ";\n"
    System.IO.File.WriteAllText(fileName, !content)

[<EntryPoint>]
let main argv =
    // Test Vector Add
    let runner = new KernelRunner()

    let a = Array.create (2 <<< 10) 2.0f 
    let b = Array.create (2 <<< 10) 2.0f
    let c = Array.zeroCreate<float32> (2 <<< 10)
    runner.Run(<@ VectorAdd(a, b, c) @>, [| a.Length |], [| 128 |])

    // Test conversion with new pipeline
    //let oldel1 = FSCL.KernelBinding.Compile(<@ MatrixMult @>)
    //let oldel = FSCL.KernelBinding.Compile(<@ Reduce @>)
    
    // Test InstructionMetric evaluation
    (*let instructionMetric = InstructionEnergyMetric("131.114.88.115") 
    let evaluation = instructionMetric.Evaluate([], <@ Convolution @>)
    let convInput = Array2D.create 10 10 2.0f
    let convFilter = Array2D.create 3 3 1.0f
    let convOutput = Array2D.create 10 10 2.0f
    let convBlock = Array2D.zeroCreate<float32> 8 8
    let instantiation = instructionMetric.Instantiate([], evaluation, <@ Convolution(convInput, convFilter, convOutput, convBlock) @>, ([| 10; 10 |], [| 5; 5 |]))
    
    // Dump instruction energy profiling
    let instructionMetric = InstructionEnergyMetric("131.114.88.115") 
    instructionMetric.DumpFolder <- Some("Dump")
    instructionMetric.MinInstr <- 1
    instructionMetric.MaxInstr <- 10000
    instructionMetric.Step <- 1000
    instructionMetric.PerStepDuration <- 15000
    instructionMetric.ThreadCount <- 2048L
    for device in ComputePlatform.Platforms.[0].Devices do
        instructionMetric.Profile(device) |> ignore
        
    // Dump memory transfer energy profiling
    let transferMetric = TransferEnergyMetric("131.114.88.115") 
    transferMetric.Validate <- true
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
        *)
    // Test vector addition
    
               (*
    // Test vector reduction
    let redA = Array.create 1024 10
    let redB = Array.zeroCreate<int> 128
    let redC = Array.zeroCreate<int> 1024
    runner.Run(<@ Reduce(redA, redB, 1024, redC) @>, [| 1024 |], [| 128 |])

    // Test matrix multiplication
    let matA = Array2D.create 64 64 2.0f 
    let matB = Array2D.create 64 64 2.0f
    let matC = Array2D.zeroCreate<float32> 64 64
    runner.Run(<@ MatrixMult(matA, matB, matC) @>, 
               [| matA.GetLength(0); matA.GetLength(1) |], [| 8; 8 |])
    *)
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
