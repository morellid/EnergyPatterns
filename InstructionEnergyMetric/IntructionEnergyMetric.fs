namespace InstructionEnergy

open MetricBase
open Cloo
open Microsoft.FSharp.Quotations
open System
open Microsoft.FSharp.Reflection
open EnergyPatterns.RemoteAmmeter
open System.IO
open EnergyMetric

// The below one needs PowerPack :(
open Microsoft.FSharp.Linq.QuotationEvaluation

type EnergyProfilingResult = (int * double * double * int64 * int) list
type EnergyInstantiationResult = double
type EnergyEvaluationResult = Expr
// Local and global sizes
type EnergyCustomData = int array * int array

type InstructionEnergyMetric(ammeterIp) =
    inherit AbsoluteMetric<ComputeDevice, EnergyProfilingResult, EnergyEvaluationResult, EnergyInstantiationResult, EnergyCustomData>()

    let mutable min_instr = 1
    let mutable max_instr = 1
    let mutable step = 1
    let mutable per_step_duration = 0
    let mutable thread_count = 128L

    member val AmmeterIp = ammeterIp with get, set

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
            let computeProgram = new ComputeProgram(computeContext, [| Tools.KernelBuilder.BuildLoopKernel() |])
            computeProgram.Build(devices, "", null, System.IntPtr.Zero)
            let computeKernel = computeProgram.CreateKernel("run")
            let computeQueue = new ComputeCommandQueue(computeContext, device, ComputeCommandQueueFlags.OutOfOrderExecution)
            let inputBuffer = new ComputeBuffer<float>(computeContext, ComputeMemoryFlags.ReadOnly, 4L)
            let outputBuffer = new ComputeBuffer<float>(computeContext, ComputeMemoryFlags.WriteOnly, 4L)
            computeKernel.SetMemoryArgument(0, inputBuffer)
            computeKernel.SetMemoryArgument(1, outputBuffer)
            computeQueue.WriteToBuffer([| 1.0 |], inputBuffer, true, null) 
            // Only for loop kernel
            computeKernel.SetValueArgument(2, currInstr / 2)

            // Run kernel n times to guarantee a total time >= PerStepDuration
            let (endMessage, time, iterations) = Tools.GetEnergyConsumption (this.AmmeterIp) 20000.0 (fun () ->
                computeQueue.Execute(computeKernel, [| 0L |], [| this.ThreadCount |], [|  Math.Min(128L, this.ThreadCount) |], null) 
                computeQueue.Finish())

            // Energy per instruction
            let avgEnergy = Double.TryParse(endMessage.Replace(",", "."))
            match avgEnergy with
            | (true, v) ->
                let energyPerInstr = ((v / 1000.0) * (double)time) / ((double)currInstr)
                result <- result @ [ (currInstr, v, energyPerInstr, time, iterations) ]
            | (false, _) ->
                let t = 0
                ()

        // Dump on file if enable 
        let content = ref "Instructions,AvgEnergy,EnergyPerInstruction,Duration,Iterations;\n"
        List.iter (fun (instr:int,avgen,en:float,time,iterations) ->
            content := !content + instr.ToString() + "," + avgen.ToString() + "," + en.ToString() + "," + time.ToString() + "," + iterations.ToString() + ";\n") result
        this.Dump("Profiling-" + this.GetType().Name + "-" + device.Name.Replace(' ', '_') + ".csv", !content) 
        result
            
            
    override this.Evaluate(profiling, expr:Expr) =
        let kernel = Tools.KernelTools.ExtractKernelDefinition(expr)
        let count = Tools.InstructionCountEstimator.EstimateInstructionCount(kernel)
        if (count.IsSome) then
            let result = count.Value
            this.Dump("Evaluate-" + this.GetType().Name + "-" + kernel.Name + ".csv", result.ToString()) 
            result
        else
            raise (MetricBase.Exceptions.MetricEvaluationError("Cannot evaluate instruction count\n"))
                
    override this.Instantiate(profiling, evaluation, invocation, (globalSize, localSize)) =
        // Evaluation is something like "<compute instruction>"
        // To compute instructions we bind the variables that are free in <compute instruction>
        let (methodInfo, args) = MetricBase.Tools.KernelTools.ExtractKernelInvocation(invocation)
        let parameters = methodInfo.GetParameters()
        let mutable freeVars = evaluation.GetFreeVars()

        // Assign values to parameter references
        let mutable finalExpr = evaluation
        for var in freeVars do
            let pIndex = Array.tryFindIndex (fun (p:Reflection.ParameterInfo) -> p.Name = var.Name) parameters
            if pIndex.IsSome then
                finalExpr <- Expr.Let(var, args.[pIndex.Value], finalExpr)
                // Remove var free ones                
                freeVars <- Seq.skip 1 freeVars
                                      
        // Assign values to fscl call placeholders
        let groups = Array.mapi (fun i el ->  el / localSize.[i]) globalSize
        for var in freeVars do
            if var.Name.StartsWith "get_global_id" then
                finalExpr <- Expr.Let(var, <@ Array.zeroCreate<int> 3 @>, finalExpr)
            if var.Name.StartsWith "get_local_id" then
                finalExpr <- Expr.Let(var, <@ Array.zeroCreate<int> 3 @>, finalExpr)
            if var.Name.StartsWith "get_global_size" then
                finalExpr <- Expr.Let(var, <@ globalSize @>, finalExpr)
            if var.Name.StartsWith "get_local_size" then
                finalExpr <- Expr.Let(var, <@ localSize @>, finalExpr)
            if var.Name.StartsWith "get_num_groups" then 
                finalExpr <- Expr.Let(var, <@ groups @>, finalExpr)
            if var.Name.StartsWith "get_work_dim" then 
                finalExpr <- Expr.Let(var, <@ groups.Rank @>, finalExpr)           
                
        let result = finalExpr.EvalUntyped()
        
        // Dump on file if enable
        let mutable content = String.concat "," (Seq.ofList (List.mapi (fun i (e:Expr) -> "Parameter" + i.ToString()) args)) + ";\n"
        content <- content + String.concat "," (Seq.ofList (List.map (fun (e:Expr) -> e.ToString()) args)) + ";\n"
        content <- content + String.concat "," (seq { for i = 0 to globalSize.Length - 1 do yield "Global size " + i.ToString() }) + ";\n"
        content <- content + String.concat "," (seq { for i = 0 to globalSize.Length - 1 do yield globalSize.[i].ToString() }) + ";\n"
        content <- content + String.concat "," (seq { for i = 0 to localSize.Length - 1 do yield "Local size " + i.ToString() }) + ";\n"
        content <- content + String.concat "," (seq { for i = 0 to localSize.Length - 1 do yield localSize.[i].ToString() }) + ";\n"
        content <- content + "Result;\n"
        content <- content + result.ToString() + ";\n"
        this.Dump("Instantiate-" + this.GetType().Name + "-" + methodInfo.Name + ".csv", content)

        result :?> double
            
