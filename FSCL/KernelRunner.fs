namespace FSCL

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection

type KernelRunner() =
    static member Init() =
        let assembly = Assembly.GetExecutingAssembly()
        assembly

    static member Run(expr: Expr, globalSize, localSize) =
        let kernel = KernelBinding.ConvertToCLKernel(expr)
        match kernel with
        | Some(kernelSource, args, i) ->
            // Something should be moved in global scope
            let platform = ComputePlatform.Platforms.[0]
            let device = platform.Devices.[0]
            let contextProperties = new ComputeContextPropertyList(platform)
            let devices = new System.Collections.Generic.List<ComputeDevice>()
            devices.Add(device)
            let computeContext = new ComputeContext(devices, contextProperties, null, System.IntPtr.Zero)            
            let computeProgram = new ComputeProgram(computeContext, kernelSource)
            computeProgram.Build(devices, "", null, System.IntPtr.Zero)
            let computeKernel = computeProgram.CreateKernel("run")

            let computeQueue = new ComputeCommandQueue(computeContext, device, ComputeCommandQueueFlags.OutOfOrderExecution)
            let inputBuffer = new ComputeBuffer<float>(computeContext, ComputeMemoryFlags.ReadOnly, 4L)
            let outputBuffer = new ComputeBuffer<float>(computeContext, ComputeMemoryFlags.WriteOnly, 4L)
            computeKernel.SetMemoryArgument(0, inputBuffer)
            computeKernel.SetMemoryArgument(1, outputBuffer)
            computeQueue.WriteToBuffer([| 1.0 |], inputBuffer, true, null) 
            computeQueue.Execute(computeKernel, [| 0L |], globalSize, localSize, null) 
            computeQueue.Finish()
        | _ ->
            ()

            
            
            
            

