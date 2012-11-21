namespace TransferEnergy

open MetricBase
open MetricBase.Tools
open MetricBase.Exceptions

open Cloo
open Microsoft.FSharp.Quotations
open System
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Linq.QuotationEvaluation
    
type EnergyProfilingResult = (int * double) list
type EnergyInstantiationResult = double

type TransferEnergyMetric() =
    let mutable min_size = 1
    let mutable max_size = 1
    let mutable step = 1
    let mutable per_step_duration = 0
    let mutable validate = false
    let mutable sourceInfo = new Data.TransferEndpoint()
    let mutable destInfo = new Data.TransferEndpoint()

    member this.MinSize
        with get() = min_size
        and set instr = min_size <- instr

    member this.MaxSize
        with get() = max_size
        and set instr = max_size <- instr
        
    member this.Step 
        with get() = step
        and set instr = step <- instr

    member this.PerStepDuration 
        with get() = per_step_duration
        and set duration = per_step_duration <- duration
       
    member this.SrcInfo 
        with get() = sourceInfo
        and set info = sourceInfo <- info

    member this.DstInfo 
        with get() = destInfo
        and set info = destInfo <- info

    member this.Validate 
        with get() = validate
        and set valid = validate <- valid

    interface AbsoluteMetric<ComputeDevice, EnergyProfilingResult, EnergyInstantiationResult> with
        member this.Profile(device) =
            // Setup CL
            let computePlatform = device.Device.Platform;
            let contextProperties = new ComputeContextPropertyList(computePlatform)
            let devices = new System.Collections.Generic.List<ComputeDevice>();
            devices.Add(device.Device)
            let computeContext = new ComputeContext(devices, contextProperties, null, System.IntPtr.Zero);
            let computeQueue = new ComputeCommandQueue(computeContext, device.Device, ComputeCommandQueueFlags.OutOfOrderExecution)
            
            // Calculate list of buffer sizes
            let bufferSizes = seq { 
                                    let i = ref this.MinSize
                                    while !i <= this.MaxSize do 
                                        yield !i
                                        i := !i + this.Step
                                  }

            // For each instr count run the test
            for currSize in bufferSizes do
                if this.SrcInfo.IsHostPtr then
                    if this.DstInfo.IsHostPtr then
                        TransferEnergy.Tools.TransferTools.HostPtrToHostPtr(currSize, this.Validate)
                    else
                       TransferEnergy.Tools.TransferTools.HostPtrToBuffer(computeContext, computeQueue, currSize, this.Validate, this.DstInfo)
                elif this.DstInfo.IsHostPtr then
                    TransferEnergy.Tools.TransferTools.BufferToHostPtr(computeContext, computeQueue, currSize, this.Validate, this.SrcInfo)                    
                else  
                    TransferEnergy.Tools.TransferTools.BufferToBuffer(computeContext, computeQueue, currSize, this.Validate, this.SrcInfo, this.DstInfo)       
                    
            new ProfilingResult<EnergyProfilingResult>([])
            
        member this.Evaluate(profiling, expr:Expr) =
            let kernel = KernelTools.ExtractKernelDefinition(expr)
            <@@ 2 @@>
                
        member this.Instantiate(evaluation, invocation) =
            // Evaluation is something like "<compute instruction>"
            // To compute instructions we bind the variables that are free in <compute instruction>
            let (methodInfo, args) = KernelTools.ExtractKernelInvocation(invocation)
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
            

