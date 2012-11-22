namespace TransferEnergy

open MetricBase
open MetricBase.Tools
open MetricBase.Exceptions

open Cloo
open Microsoft.FSharp.Quotations
open System
open Microsoft.FSharp.Reflection
open System.Reflection
open Microsoft.FSharp.Linq.QuotationEvaluation
open System.Diagnostics
open TransferEnergy.Tools.TransferTools
open EnergyPatterns.RemoteAmmeter
open System.Runtime.InteropServices

type EnergyProfilingResult = (int * double) list
type EnergyInstantiationResult = double
type EnergyEvaluationResult = (ParameterInfo * Data.BufferAccess) []

type TransferEnergyMetric(ammeterIp:string) =
    let mutable min_size = 1
    let mutable max_size = 1
    let mutable step = 1
    let mutable per_step_duration = 0
    let mutable validate = false
    let mutable sourceInfo = new Data.TransferEndpoint()
    let mutable destInfo = new Data.TransferEndpoint()

    member val AmmeterIp = ammeterIp with get, set
        
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

    interface AbsoluteMetric<ComputeDevice, EnergyProfilingResult, EnergyEvaluationResult, EnergyInstantiationResult> with
        member this.Profile(device) =
            // Create ammeter client
            let client = new Client(this.AmmeterIp)

            // Create result
            let mutable result = []

            // Setup CL
            let computePlatform = device.Platform;
            let contextProperties = new ComputeContextPropertyList(computePlatform)
            let devices = new System.Collections.Generic.List<ComputeDevice>();
            devices.Add(device)
            let computeContext = new ComputeContext(devices, contextProperties, null, System.IntPtr.Zero);
            let computeQueue = new ComputeCommandQueue(computeContext, device, ComputeCommandQueueFlags.OutOfOrderExecution)
            
            // Calculate list of buffer sizes
            let bufferSizes = seq { 
                                    let i = ref this.MinSize
                                    while !i <= this.MaxSize do 
                                        yield !i
                                        i := !i + this.Step
                                  }

            // For each instr count run the test of allocation, initialization and transferring
            for currSize in bufferSizes do
                // Allocate and init src, allocate dst
                let mutable srcPtr = None
                let mutable dstPtr = None
                let mutable srcBuffer = None
                let mutable dstBuffer = None
                if this.SrcInfo.IsHostPtr then
                    srcPtr <- Some(AllocateHostPtr(currSize))
                    do InitializeHostPtr(currSize, srcPtr.Value)
                else
                    srcBuffer <- Some(AllocateBuffer(computeContext, currSize, this.SrcInfo))
                    do InitializeBuffer(computeQueue, currSize, this.SrcInfo, srcBuffer.Value)
                if this.DstInfo.IsHostPtr then
                    dstPtr <- Some(AllocateHostPtr(currSize))
                else
                    dstBuffer <- Some(AllocateBuffer(computeContext, currSize, this.DstInfo))

                // Determine number of iterations to guarantee per step duration
                let timer = Stopwatch()
                timer.Start()                        
                if this.SrcInfo.IsHostPtr then
                    if this.DstInfo.IsHostPtr then
                        HostPtrToHostPtr(currSize, this.Validate, srcPtr.Value, dstPtr.Value)
                    else
                        HostPtrToBuffer(computeContext, computeQueue, currSize, this.Validate, this.DstInfo, srcPtr.Value, dstBuffer.Value)
                elif this.DstInfo.IsHostPtr then
                    BufferToHostPtr(computeContext, computeQueue, currSize, this.Validate, this.SrcInfo, srcBuffer.Value, dstPtr.Value)  
                else  
                    BufferToBuffer(computeContext, computeQueue, currSize, this.Validate, this.SrcInfo, this.DstInfo, srcBuffer.Value, dstBuffer.Value)       
                timer.Stop()
                            
                // Execute profiling (excluding allocation)
                let iterations = (int) ((double)this.PerStepDuration * 10.0 / ((double)timer.ElapsedMilliseconds))
                
                timer.Reset()
                let startResult = client.start() 
                timer.Start()                              
                for i in 0 .. iterations - 1 do     
                    if this.SrcInfo.IsHostPtr then
                        if this.DstInfo.IsHostPtr then
                            HostPtrToHostPtr(currSize, this.Validate, srcPtr.Value, dstPtr.Value)
                        else
                            HostPtrToBuffer(computeContext, computeQueue, currSize, this.Validate, this.DstInfo, srcPtr.Value, dstBuffer.Value)
                    elif this.DstInfo.IsHostPtr then
                        BufferToHostPtr(computeContext, computeQueue, currSize, this.Validate, this.SrcInfo, srcBuffer.Value, dstPtr.Value)  
                    else  
                        BufferToBuffer(computeContext, computeQueue, currSize, this.Validate, this.SrcInfo, this.DstInfo, srcBuffer.Value, dstBuffer.Value)       
                timer.Stop()
                let endResult = client.stop()
                
                // Calculate energy per byte transferred
                let avgEnergy = Double.Parse(endResult.Replace(",", "."))
                let energyPerByte = ((avgEnergy / 1000.0) * (double)timer.ElapsedMilliseconds) / ((double)currSize)
                result <- result @ [ (currSize, energyPerByte) ]

            result

        member this.Evaluate(profiling, expr:Expr) =
            let kernel = KernelTools.ExtractKernelDefinition(expr)
            let parmsAccess = Tools.ParameterAccessAnalyzer.Analyze(kernel)
            parmsAccess
                
        member this.Instantiate(profiling, evaluation, invocation) =            
            let (methodInfo, args) = MetricBase.Tools.KernelTools.ExtractKernelInvocation(invocation)
            let parameters = methodInfo.GetParameters()
            let totalBytes = ref 0
            Array.iteri (fun i (p:ParameterInfo) ->
                if p.ParameterType.IsArray then
                    let v = args.[i].EvalUntyped()
                    // Get length
                    let elements = p.ParameterType.GetProperty("Length").GetValue(v) :?> int
                    totalBytes := !totalBytes + (elements * Marshal.SizeOf(p.ParameterType.GetElementType()))) parameters

            (double)!totalBytes
                
            

