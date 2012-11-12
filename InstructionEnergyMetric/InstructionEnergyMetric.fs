namespace InstructionEnergyMetric

open MetricUtil
open Cloo
open Microsoft.FSharp.Quotations
open System
open Microsoft.FSharp.Reflection
// The below one needs PowerPack :(
open Microsoft.FSharp.Linq.QuotationEvaluation

type EnergyProfilingResult = (int * double) list
type EnergyInstantiationResult = double

type InstructionEnergyMetric() =
    let mutable min_instr = 1
    let mutable max_instr = 1
    let mutable step = 1
    let mutable per_step_duration = 0
    let mutable thread_count = 128L

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

    interface AbsoluteMetric<ComputeDevice, EnergyProfilingResult, EnergyInstantiationResult> with
        member this.Profile(device) =
            // Setup CL
            let computePlatform = device.Device.Platform;
            let contextProperties = new ComputeContextPropertyList(computePlatform)
            let devices = new System.Collections.Generic.List<ComputeDevice>();
            devices.Add(device.Device)
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
                let computeProgram = new ComputeProgram(computeContext, KernelBuilder.BuildKernel(currInstr))
                computeProgram.Build(devices, "", null, System.IntPtr.Zero)
                let computeKernel = computeProgram.CreateKernel("run")
                let computeQueue = new ComputeCommandQueue(computeContext, device.Device, ComputeCommandQueueFlags.OutOfOrderExecution)
                let inputBuffer = new ComputeBuffer<float>(computeContext, ComputeMemoryFlags.ReadOnly, 4L)
                let outputBuffer = new ComputeBuffer<float>(computeContext, ComputeMemoryFlags.WriteOnly, 4L)
                computeKernel.SetMemoryArgument(0, inputBuffer)
                computeKernel.SetMemoryArgument(1, outputBuffer)
                computeQueue.WriteToBuffer([| 1.0 |], inputBuffer, true, null) 

                // Run kernel n times to guarantee a total time >= PerStepDuration
                let timer = System.Diagnostics.Stopwatch()
                timer.Start()
                for i in 0 .. 10 do
                    computeQueue.Execute(computeKernel, [| 0L |], [| this.ThreadCount |], [|  Math.Min(128L, this.ThreadCount) |], null) 
                    computeQueue.Finish()
                timer.Stop()

                let iterations = (int) ((double)this.PerStepDuration * 10.0 / ((double)timer.ElapsedMilliseconds))
                for i in 0 .. iterations - 1 do
                    computeQueue.Execute(computeKernel, [| 0L |], [| this.ThreadCount |], [|  Math.Min(128L, this.ThreadCount) |], null) 
                    computeQueue.Finish()
            
            new ProfilingResult<EnergyProfilingResult>([])
            
        member this.Evaluate(profiling, expr:Expr) =
            let kernel = MetricTools.ExtractKernelDefinition(expr)
            let count = InstructionCountEstimator.EstimateInstructionCount(kernel)
            if (count.IsSome) then
                count.Value
            else
                raise (MetricEvaluationError("Cannot evaluate instruction count\n"))
                
        member this.Instantiate(evaluation, invocation) =
            // Evaluation is something like "<compute instruction>"
            // To compute instructions we bind the variables that are free in <compute instruction>
            let (methodInfo, args) = MetricTools.ExtractKernelInvocation(invocation)
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
            new InstantiationResult<double>(result :?> double)
            
