namespace ComputationDensity

open MetricBase
open Cloo
open Microsoft.FSharp.Quotations
open System
open Microsoft.FSharp.Reflection
open System.IO
open MetricTools
open MetricTools.MemoryTransfer
open MetricTools.MemoryAccess
open System.Reflection
open System.Collections.Generic
open System.Runtime.InteropServices

// The below one needs PowerPack :(
open Microsoft.FSharp.Linq.QuotationEvaluation

type ProfilingResult = (int64 * float) list
type InstantiationResult = int
type EvaluationResult = (Expr * Dictionary<ParameterInfo, BufferAccess>)
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
        let inputPtr = Array.create ((int)transferSize / sizeof<float32>) 5.0f
        let inputBuffer = new ComputeBuffer<float32>(context, ComputeMemoryFlags.ReadOnly, transferSize / (int64 sizeof<float32>))
        let outputBuffer = new ComputeBuffer<float32>(context, ComputeMemoryFlags.WriteOnly, 1L)
        computeKernel.SetMemoryArgument(0, inputBuffer)
        computeKernel.SetMemoryArgument(1, outputBuffer)
        // Only for loop kernel
        computeKernel.SetValueArgument(2, currInstr / 2)
        computeKernel.SetValueArgument(3, 0)

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
            let rightInstr = ref (this.MaxInstr)
            let leftInstr = ref 0
            let currInstr = ref ((!rightInstr + !leftInstr) / 2)

            // Binary search
            while not thresholdFound do         
                let currThread = this.MaxThread
                let executionResult = List.map(fun (d, c) ->
                    this.RunKernel(transferSize, !currInstr, currThread, d, c)) [(gpu, gpuContext); (apu, apuContext)]

                let ratio = executionResult.[0] / executionResult.[1]
                if (Math.Abs((double) ratio - 1.0) < 0.01) || (!currInstr = 1) || (!currInstr = this.MaxInstr) then
                    thresholdFound <- true
                else
                    if ratio > 1.0 then
                        leftInstr := !currInstr
                    else
                        rightInstr := !currInstr
                    currInstr := ((!rightInstr + !leftInstr) / 2)

            let threshold = (double)!currInstr / (double)transferSize
            result <- result @ [(transferSize, threshold)]

        result
                                
    override this.Evaluate(profiling, expr:Expr) =
        let kernel = Tools.KernelTools.ExtractKernelDefinition(expr)
        let counter = new Counter(kernel)
        let instructions = counter.Count(this.OnNewOperation counter, true)
        let access = AccessAnalyzer.Analyze(kernel)
        (instructions, access)
            
    override this.Instantiate(profiling, evaluation, invocation, (globalSize, localSize)) =
        match evaluation with
        | instructions, access ->
            // Evaluation is something like "<compute instruction>"
            // To compute instructions we bind the variables that are free in <compute instruction>
            let (methodInfo, args) = MetricBase.Tools.KernelTools.ExtractKernelInvocation(invocation)
            let parameters = methodInfo.GetParameters()
            let mutable freeVars = instructions.GetFreeVars()
            // Assign values to parameter references
            let mutable finalExpr = instructions
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
            let actualInstructions = result :?> double

            // Determine transfer size
            let parameters = methodInfo.GetParameters()
            let bytesToKernel = ref 0
            let bytesFromKernel = ref 0
            Array.iteri (fun i (p:ParameterInfo) ->
                if p.ParameterType.IsArray then
                    let v = args.[i].EvalUntyped()
                    // Get length
                    let elements = p.ParameterType.GetProperty("Length").GetValue(v) :?> int
                    match access.[p] with
                    | READ_ONLY 
                    | READ_WRITE ->
                        bytesToKernel := !bytesToKernel + (elements * Marshal.SizeOf(p.ParameterType.GetElementType()))
                    | WRITE_ONLY
                    | READ_WRITE ->
                        bytesFromKernel := !bytesFromKernel + (elements * Marshal.SizeOf(p.ParameterType.GetElementType()))
                    | NO_ACCESS ->
                        ()) parameters
                
            // Dump on file if enable
            let mutable content = "Instructions;BytesTransferredToKernel;BytesTransferredFromKernel\n"
            content <- content + actualInstructions.ToString() + ";" + (!bytesToKernel).ToString() + ";" + (!bytesFromKernel).ToString() + "\n"
            this.Dump("Instantiate-" + this.GetType().Name + "-" + methodInfo.Name + ".csv", content)

            // Check the threshold for the given transfer size
            let mutable index = 0
            let mutable found = false
            while (not found) && (index < profiling.Length) do
                let transferSize, _ = profiling.[index]
                if transferSize >= (int64)!bytesToKernel + (int64)!bytesFromKernel then
                    found <- true
                else
                    index <- index + 1
            if not found then
                index <- profiling.Length - 1

            // Return the best device
            let _, th = profiling.[index]
            if actualInstructions > th then
                0
            else
                1
                
            
