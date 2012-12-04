namespace ComputationDensity

open MetricBase
open Cloo
open Microsoft.FSharp.Quotations
open System
open Microsoft.FSharp.Reflection
open System.IO
open ExpressionCounter
open Tools

// The below one needs PowerPack :(
open Microsoft.FSharp.Linq.QuotationEvaluation

type ProfilingResult = (int64 * float) list
type InstantiationResult = ComputeDevice
type EvaluationResult = Expr
// Local and global sizes
type EnergyCustomData = int array * int array

type ComputationDensityMetric() =
    inherit RelativeMetric<ComputeDevice, ProfilingResult, EvaluationResult, InstantiationResult, EnergyCustomData>()

    let mutable max_instr = 1
    let mutable min_thread = 1L
    let mutable max_thread = 1L
    let mutable min_transfer = 1024L
    let mutable max_transfer = (32L <<< 20)
    let mutable transfer_step = (fun (i:int64) -> i * 2L)
    let mutable instr_step = (fun (i:int) -> i * 2)
    let mutable thread_step = (fun (i:int64) -> i * 2L)
    let mutable per_step_duration = 0.0

    member this.MaxInstr 
        with get() = max_instr
        and set instr = max_instr <- instr
        
    member this.MinThread 
        with get() = min_thread
        and set th = min_thread <- th

    member this.MaxThread 
        with get() = max_thread
        and set th = max_thread <- th
        
    member this.MinTransfer
        with get() = min_transfer
        and set th = min_transfer <- th

    member this.MaxTransfer
        with get() = max_transfer
        and set th = max_transfer <- th

    member this.InstrStep 
        with get() = instr_step
        and set instr = instr_step <- instr
        
    member this.ThreadStep 
        with get() = thread_step
        and set instr = thread_step <- instr
        
    member this.TransferStep 
        with get() = transfer_step
        and set instr = transfer_step <- instr

    member this.PerStepDuration 
        with get() = per_step_duration
        and set duration = per_step_duration <- duration

    member private this.OnNewOperation (counter:Counter) (expr, parameters:Reflection.ParameterInfo[]) =
        match expr with
        | DerivedPatterns.SpecificCall <@ (+) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (-) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (*) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (/) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (%) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (&&) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (||) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (&&&) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (|||) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (<<<) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (>>>) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (^^^) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (~-) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (~+) @> (e, t, a) ->
            let result = ref <@ 1.0 @>
            for arg in a do
                let sub = counter.ContinueCount(arg)
                result := <@ (%(!result) : double) + (%%sub : double) @>
            Value(!result)
        | _ ->
            Continue

    member private this.OnNewRead (counter:Counter) (expr, parameters:Reflection.ParameterInfo[]) =
        match expr with
        | Patterns.Call(o, mi, args) ->            
            if mi.DeclaringType.Name = "IntrinsicFunctions" && mi.Name.StartsWith "GetArray" then
                match args.[0] with
                | Patterns.Var(v) ->
                    let result = ref <@ 1.0 @>
                    for arg in args do
                        let sub = counter.ContinueCount(arg)
                        result := <@ (%(!result) : double) + (%%sub : double) @>
                    Value(!result)
                | _ ->
                    Continue
            else
                Continue
        | _ ->
            Continue
                
    member private this.OnNewWrite (counter:Counter) (expr, parameters:Reflection.ParameterInfo[]) =
        match expr with
        | Patterns.Call(o, mi, args) ->            
            if mi.DeclaringType.Name = "IntrinsicFunctions" && mi.Name.StartsWith "SetArray" then
                match args.[0] with
                | Patterns.Var(v) ->
                    let result = ref <@ 1.0 @>
                    for arg in args do
                        let sub = counter.ContinueCount(arg)
                        result := <@ (%(!result) : double) + (%%sub : double) @>
                    Value(!result)
                | _ ->
                    Continue
            else
                Continue
        | _ ->
            Continue     

    member private this.RunKernel (transferSize:int64, currInstr:int, currThread:int64, device, context:ComputeContext) =       
        let devices = new System.Collections.Generic.List<ComputeDevice>();
        devices.Add(device) 

        //for currThread in threadCount do
        let computeProgram = new ComputeProgram(context, [| Tools.KernelBuilder.BuildLoopInstructionKernel() |])
        computeProgram.Build(devices, "", null, System.IntPtr.Zero)
        let computeKernel = computeProgram.CreateKernel("run")
        let computeQueue = new ComputeCommandQueue(context, device, ComputeCommandQueueFlags.OutOfOrderExecution)
        let inputPtr = Array.create ((int)transferSize / sizeof<float32>) 1.0f
        let inputBuffer = new ComputeBuffer<float32>(context, ComputeMemoryFlags.ReadOnly, transferSize)
        let outputBuffer = new ComputeBuffer<float32>(context, ComputeMemoryFlags.WriteOnly, 4L)
        computeKernel.SetMemoryArgument(0, inputBuffer)
        computeKernel.SetMemoryArgument(1, outputBuffer)
        // Only for loop kernel
        computeKernel.SetValueArgument(2, currInstr / 2)

        // Run kernel n times to guarantee a total time >= PerStepDuration
        let (_, time, iterations) = Tools.ExcuteFor (this.PerStepDuration) (fun () -> "OK") (fun () -> ()) (fun () ->
            computeQueue.WriteToBuffer(inputPtr, inputBuffer, true, null) 
            computeQueue.Execute(computeKernel, [| 0L |], [| currThread |], [|  Math.Min(128L, currThread) |], null) 
            computeQueue.Finish())
                        
        (double)time / (double)iterations

    override this.Profile(devs:ComputeDevice list) =        
        let mutable result = []

        // Two devices: the second is an integrated GPU (but we should generalize)
        let gpu = devs.[0]
        let apu = devs.[1]

        // Setup CL
        let computePlatform = gpu.Platform;
        let contextProperties = new ComputeContextPropertyList(computePlatform)
        let devices = new System.Collections.Generic.List<ComputeDevice>();
        devices.Add(gpu)
        let gpuContext = new ComputeContext(devices, contextProperties, null, System.IntPtr.Zero);
        devices.RemoveAt(0)
        devices.Add(apu)
        let apuContext = new ComputeContext(devices, contextProperties, null, System.IntPtr.Zero);
            
        // Calculate list of thread count
        let threadCount = seq { 
                                let i = ref this.MinThread
                                while !i <= this.MaxThread do 
                                    yield !i
                                    i := this.ThreadStep !i
                              }

        // Calculate list of transfer count
        let transferCount = seq { 
                                let i = ref this.MinTransfer
                                while !i <= this.MaxTransfer do 
                                    yield !i
                                    i := this.TransferStep !i
                                }

        // For each transfer size run the test
        for transferSize in transferCount do
            // For each instr count run the test
            let mutable thresholdFound = false
            let mutable currInstr = this.MaxInstr / 2
            let mutable prevInstr = 0

            // Binary search
            while not thresholdFound do         
                let currThread = this.MaxThread
                let executionResult = List.map(fun (d, c) ->
                    this.RunKernel(transferSize, currInstr, currThread, d, c)) [(gpu, gpuContext); (apu, apuContext)]

                let ratio = executionResult.[0] / executionResult.[1]
                if (Math.Abs((double) ratio - 1.0) < 0.01) then
                    thresholdFound <- true
                else
                    if ratio > 1.0 then
                        prevInstr <- currInstr
                        currInstr <- (currInstr + this.MaxInstr) / 2
                    else
                        prevInstr <- currInstr
                        currInstr <- (currInstr + prevInstr) / 2

            let threshold = (double)currInstr / (double)transferSize
            result <- result @ [(transferSize, threshold)]

        result

                                
    override this.Evaluate(profiling, expr:Expr) =
        let kernel = Tools.KernelTools.ExtractKernelDefinition(expr)
        let counter = new Counter(kernel)
        let instructions = counter.Count(this.OnNewOperation counter, true)
        instructions
            
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

        result :?> double) [ instructions; reads; writes ]
        
            // Dump on file if enable
            let mutable content = String.concat "," (Seq.ofList (List.mapi (fun i (e:Expr) -> "Parameter" + i.ToString()) args)) + ";\n"
            content <- content + String.concat "," (Seq.ofList (List.map (fun (e:Expr) -> e.ToString()) args)) + ";\n"
            content <- content + String.concat "," (seq { for i = 0 to globalSize.Length - 1 do yield "Global size " + i.ToString() }) + ";\n"
            content <- content + String.concat "," (seq { for i = 0 to globalSize.Length - 1 do yield globalSize.[i].ToString() }) + ";\n"
            content <- content + String.concat "," (seq { for i = 0 to localSize.Length - 1 do yield "Local size " + i.ToString() }) + ";\n"
            content <- content + String.concat "," (seq { for i = 0 to localSize.Length - 1 do yield localSize.[i].ToString() }) + ";\n"
            content <- content + "Instructions,Reads,Writes;\n"
            content <- content + result.[0].ToString() + "," + result.[1].ToString() + "," + result.[2].ToString() + ";\n"
            this.Dump("Instantiate-" + this.GetType().Name + "-" + methodInfo.Name + ".csv", content)

            (result.[0], result.[1], result.[2])
            
