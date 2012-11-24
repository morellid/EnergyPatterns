namespace InstructionEnergy

open MetricBase
open Cloo
open Microsoft.FSharp.Quotations
open System
open Microsoft.FSharp.Reflection
open EnergyPatterns.RemoteAmmeter
open System.IO

// The below one needs PowerPack :(
open Microsoft.FSharp.Linq.QuotationEvaluation

type EnergyProfilingResult = (int * double) list
type EnergyInstantiationResult = double
type EnergyEvaluationResult = Expr

type InstructionEnergyMetric(ammeterIp) =
    inherit AbsoluteMetric<ComputeDevice, EnergyProfilingResult, EnergyEvaluationResult, EnergyInstantiationResult>()

    let mutable min_instr = 1
    let mutable max_instr = 1
    let mutable step = 1
    let mutable per_step_duration = 0
    let mutable thread_count = 128L
    let mutable dumpFile = None

    member val AmmeterIp = ammeterIp with get, set

    member this.DumpFolder
        with get() = dumpFile
        and set v = dumpFile <- v

    member this.MinInstr 
        with get() = min_instr
        and set instr = min_instr <- instr

    member this.MaxInstr 
        with get() = max_instr
        and set instr = max_instr <- instr
        
    member this.Step 
        with get() = step
        and set instr = step <- instr

    member this.PerStepDuration 
        with get() = per_step_duration
        and set duration = per_step_duration <- duration
        
    member this.ThreadCount 
        with get() = thread_count
        and set count = thread_count <- count

    override this.Profile(device:ComputeDevice) =
        // Create ammeter client
        let client = new Client(this.AmmeterIp)

        let mutable result = []

        // Setup CL
        let computePlatform = device.Platform;
        let contextProperties = new ComputeContextPropertyList(computePlatform)
        let devices = new System.Collections.Generic.List<ComputeDevice>();
        devices.Add(device)
        let computeContext = new ComputeContext(devices, contextProperties, null, System.IntPtr.Zero);
            
        // Calculate list of instr count
        let instrCount = seq { 
                                let i = ref this.MinInstr
                                while !i <= this.MaxInstr do 
                                    yield !i
                                    i := !i + this.Step
                                }

        // For each instr count run the test
        for currInstr in instrCount do
            let computeProgram = new ComputeProgram(computeContext, [| Tools.KernelBuilder.BuildKernel(currInstr) |])
            computeProgram.Build(devices, "", null, System.IntPtr.Zero)
            let computeKernel = computeProgram.CreateKernel("run")
            let computeQueue = new ComputeCommandQueue(computeContext, device, ComputeCommandQueueFlags.OutOfOrderExecution)
            let inputBuffer = new ComputeBuffer<float>(computeContext, ComputeMemoryFlags.ReadOnly, 4L)
            let outputBuffer = new ComputeBuffer<float>(computeContext, ComputeMemoryFlags.WriteOnly, 4L)
            computeKernel.SetMemoryArgument(0, inputBuffer)
            computeKernel.SetMemoryArgument(1, outputBuffer)
            computeQueue.WriteToBuffer([| 1.0 |], inputBuffer, true, null) 

            // Run kernel n times to guarantee a total time >= PerStepDuration
            let timer = System.Diagnostics.Stopwatch()
            let mutable testIterations = 1
            let mutable reliableTest = false
            while (not reliableTest) do
                timer.Reset()
                timer.Start()
                for i in 0 .. testIterations do
                    computeQueue.Execute(computeKernel, [| 0L |], [| this.ThreadCount |], [|  Math.Min(128L, this.ThreadCount) |], null) 
                    computeQueue.Finish()
                timer.Stop()

                if (timer.ElapsedMilliseconds > 100L) then
                    reliableTest <- true
                else
                    testIterations <- testIterations * 10

            let iterations = (int) ((double)this.PerStepDuration * (double)testIterations / ((double)timer.ElapsedMilliseconds))
            timer.Reset()
            let startMessage = client.start()
            timer.Start()
            for i in 0 .. iterations - 1 do
                computeQueue.Execute(computeKernel, [| 0L |], [| this.ThreadCount |], [|  Math.Min(128L, this.ThreadCount) |], null) 
                computeQueue.Finish()
            timer.Stop()
            let endMessage = client.stop()

            // Energy per instruction
            let avgEnergy = Double.Parse(endMessage.Replace(",", "."))
            let energyPerInstr = ((avgEnergy / 1000.0) * (double)timer.ElapsedMilliseconds) / ((double)currInstr)
            result <- result @ [ (currInstr, energyPerInstr) ]

        // Dump on file if enable
        if dumpFile.IsSome then
            if not (Directory.Exists(dumpFile.Value)) then
                Directory.CreateDirectory(dumpFile.Value) |> ignore

            let fileName = dumpFile.Value + "\\" + "Profiling-" + this.GetType().Name + "-" + device.Name.Replace(' ', '_') + ".csv"  
            let content = ref "Instructions,EnergyPerInstruction;"
            List.iter (fun (instr:int,en:float) ->
                content := !content + instr.ToString() + "," + en.ToString() + ";") result
            File.WriteAllText(fileName, !content)
        result
            
            
    override this.Evaluate(profiling, expr:Expr) =
        let kernel = Tools.KernelTools.ExtractKernelDefinition(expr)
        let count = Tools.InstructionCountEstimator.EstimateInstructionCount(kernel)
        if (count.IsSome) then
            count.Value
        else
            raise (MetricBase.Exceptions.MetricEvaluationError("Cannot evaluate instruction count\n"))
                
    override this.Instantiate(profiling, evaluation:Expr, invocation) =
        // Evaluation is something like "<compute instruction>"
        // To compute instructions we bind the variables that are free in <compute instruction>
        let (methodInfo, args) = MetricBase.Tools.KernelTools.ExtractKernelInvocation(invocation)
        let parameters = methodInfo.GetParameters()
        let freeVars = Seq.toList (evaluation.GetFreeVars())

        let findByName name vl =
            List.tryFind (fun (v: Var) -> v.Name = name) vl

        let mutable finalExpr =  evaluation
        for i = 0 to args.Length - 1 do
            let freeVar = findByName (parameters.[i].Name) freeVars
            if freeVar.IsSome then
                finalExpr <- Expr.Let(freeVar.Value, args.[i], finalExpr)
                                           
        let result = finalExpr.EvalUntyped()
        
        // Dump on file if enable
        if dumpFile.IsSome then
            if not (Directory.Exists(dumpFile.Value)) then
                Directory.CreateDirectory(dumpFile.Value) |> ignore

            let fileName = dumpFile.Value + "\\" + "Instatiate-" + this.GetType().Name + "-" + methodInfo.Name + ".csv"  
            let content = ref ("Instructions;" + (result :?> double).ToString())
            File.WriteAllText(fileName, !content)

        result :?> double
            
