namespace FSCL

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open Microsoft.FSharp.Linq.QuotationEvaluation
open FSCL.Compiler.Processors
open FSCL.Compiler
open System.Collections.Generic
        
        (*
// Wrappers just for syntax purpose. You can run your kernel by typing f.WithSize(sizes).Run(params)
type KernelSizeWrapper<'T,'U>(f: Expr<'T -> 'U>, k:KernelRunner) =
    member this.WithSize(globalSize: int array, localSize: int array) =
        new KernelCallWrapper<'T,'U>(f, k, globalSize, localSize)
        
and KernelCallWrapper<'T,'U>(f: Expr<'T -> 'U>, k:KernelRunner, globalSize: int array, localSize: int array) =
    member this.Run(p:'T) =
        k.Run(f, p, globalSize, localSize) *)

type KernelDanglingBuffer(buffer:ComputeMemory, dim:int, deviceIndex:int) =
    member val Buffer = buffer with get
    member val Dimensions = dim with get
    member val DeviceIndex = deviceIndex with get

// The Kernel runner
type KernelRunner =    
    val compiler : KernelCompiler
    val DanglingBuffers : Dictionary<Expr, KernelDanglingBuffer>
        
    member this.Compiler 
        with get() = 
            this.compiler
(*
    member this.Kernel(f: Expr<'T -> 'U>) =
        new KernelSizeWrapper<'T, 'U>(f, this) 
        *)
    // Run a kernel through a quoted kernel call
    member this.Run(expr: Expr, globalSize: int array, localSize: int array) =
        let globalDataStorage = this.compiler.GlobalDataStorage
                     
        let (c, kernelInfo, args) = KernelCompilerTools.ExtractMethodInfo(expr)
        // Found a kernel in global data matching the call
        let matchingKernel = ref (List.tryFind(fun (k:FSCLKernelData) -> k.MethodInfo = kernelInfo) (this.compiler.GlobalDataStorage.Kernels))
        if (!matchingKernel).IsNone then
            // Try add it to the compiler
            matchingKernel := Some(this.compiler.Add(kernelInfo))
            
        // Fix: here to be called INSTANTIATE on a metric to get the device to use
        let kernelInstance = (!matchingKernel).Value.Instances.[0]
        let queue = globalDataStorage.Devices.[kernelInstance.DeviceIndex].Queue
        let context = globalDataStorage.Devices.[kernelInstance.DeviceIndex].Context
        // FIX: determine best read/write strategy

        // For each parameter, create buffer (if array), write it and set kernel arg   
        let additionalArgCount = ref 0     
        let paramObjectBufferMap = new Dictionary<string, (System.Object * ComputeMemory)>()

        let argIndex = ref 0
        for (par, dim, argumentExpr) in args do
            let (actualArg, actualArgOption) = KernelRunnerTools.ParseKernelCallArg(argumentExpr)

            // Check dangling buffer
            let o = actualArg.EvalUntyped() 
            let mutable buffer = None
            if par.ParameterType.IsArray then
                if actualArgOption = NOT_USED then
                    // Check if buffer contained in dangling buffers
                    if this.DanglingBuffers.ContainsKey(actualArg) then
                        buffer <- Some(this.DanglingBuffers.[actualArg].Buffer)
                              
                // Check if constant buffer. In this case we pass the dimension (sizeof) the array and not a real buffer
                if (!matchingKernel).Value.Parameters.[par.Name].AddressSpace = KernelParameterAddressSpace.LocalSpace then
                    let size = (o.GetType().GetProperty("LongLength").GetValue(o) :?> int64) * 
                                (int64 (System.Runtime.InteropServices.Marshal.SizeOf(o.GetType().GetElementType())))
                    // Set kernel arg
                    kernelInstance.Kernel.SetLocalArgument(!argIndex, size) 
                else
                    if buffer.IsNone then
                        let o = actualArg.EvalUntyped()
                        // Check if read or read_write mode
                        let matchingParameter = (!matchingKernel).Value.Parameters.[par.Name]
                        let access = matchingParameter.Access
                        let mustInitBuffer =
                            ((matchingParameter.AddressSpace = KernelParameterAddressSpace.GlobalSpace) ||
                                (matchingParameter.AddressSpace = KernelParameterAddressSpace.ConstantSpace)) &&
                            ((access = KernelParameterAccessMode.ReadOnly) || 
                                (access = KernelParameterAccessMode.ReadWrite))

                        // Create buffer and eventually init it
                        let t = par.ParameterType.GetElementType()
                        buffer <- Some(KernelRunnerTools.WriteBufferGeneric(t, context, queue, o, dim, mustInitBuffer))
                        
                        // Store association between parameter, array and buffer object
                        paramObjectBufferMap.Add(par.Name, (o, buffer.Value))

                    // Set kernel arg
                    kernelInstance.Kernel.SetMemoryArgument(!argIndex, buffer.Value)

                // Set additional args for array params (dimensions) 
                for dimension = 0 to dim - 1 do
                    let sizeOfDim = o.GetType().GetMethod("GetLength").Invoke(o, [| dimension |]) :?> int
                    kernelInstance.Kernel.SetValueArgument<int>(args.Length + !additionalArgCount + dimension, sizeOfDim)
                additionalArgCount := !additionalArgCount + dim
            else
                let o = actualArg.EvalUntyped()
                let t = par.ParameterType
                if (t = typeof<uint32>) then
                    kernelInstance.Kernel.SetValueArgument<uint32>(!argIndex, o :?> uint32)
                elif (t = typeof<uint64>) then
                    kernelInstance.Kernel.SetValueArgument<uint64>(!argIndex, o :?> uint64)
                elif (t = typeof<int64>) then
                    kernelInstance.Kernel.SetValueArgument<int64>(!argIndex, o :?> int64)
                elif (t = typeof<int>) then
                    kernelInstance.Kernel.SetValueArgument<int>(!argIndex, o :?> int)
                elif (t = typeof<double>) then
                    kernelInstance.Kernel.SetValueArgument<double>(!argIndex, o :?> double)
                elif (t = typeof<float32>) then
                    kernelInstance.Kernel.SetValueArgument<float32>(!argIndex, o :?> float32)
                elif (t = typeof<bool>) then
                    kernelInstance.Kernel.SetValueArgument<bool>(!argIndex, o :?> bool)
            
            argIndex := !argIndex + 1

        // Run kernel
        let offset = Array.zeroCreate<int64>(globalSize.Length)
        // 32 bit enought for size_t. Kernel uses size_t like int withour cast. We cannot put case into F# kernels each time the user does operations with get_global_id and similar!
        queue.Execute(kernelInstance.Kernel, offset, Array.map(fun el -> int64(el)) globalSize, Array.map(fun el -> int64(el)) localSize, null)

        // Read result if needed
        for (par, dim, argumentExpr) in args do
            let (actualArg, actualArgOption) = KernelRunnerTools.ParseKernelCallArg(argumentExpr)
            if par.ParameterType.IsArray then
                if actualArgOption = NOT_USED then
                    if not (this.DanglingBuffers.ContainsKey(actualArg)) then
                        this.DanglingBuffers.Add(actualArg, new KernelDanglingBuffer(snd(paramObjectBufferMap.[par.Name]), dim, kernelInstance.DeviceIndex))
                else
                    if (!matchingKernel).Value.Parameters.[par.Name].AddressSpace <> KernelParameterAddressSpace.LocalSpace then
                        // Get association between parameter, array and buffer object
                        let (o, buffer) = paramObjectBufferMap.[par.Name]

                        // Check if write or read_write mode
                        let mutable mustReadBuffer = false
                        let matchingParameter = (!matchingKernel).Value.Parameters.[par.Name]
                        let access = matchingParameter.Access
                        mustReadBuffer <-                     
                            ((matchingParameter.AddressSpace = KernelParameterAddressSpace.GlobalSpace)) &&
                            ((access = KernelParameterAccessMode.WriteOnly) || 
                             (access = KernelParameterAccessMode.ReadWrite))

                        if(mustReadBuffer) then
                            // Create buffer and eventually init it
                            let t = par.ParameterType.GetElementType()
                            KernelRunnerTools.ReadBufferGeneric(t, context, queue, o, dim, buffer)

    member this.Read(expr:Expr) =
        if this.DanglingBuffers.ContainsKey(expr) then
            let danglingBuffer = this.DanglingBuffers.[expr]
            let device = this.compiler.GlobalDataStorage.Devices.[danglingBuffer.DeviceIndex]
            KernelRunnerTools.ReadBufferGeneric(expr.Type.GetElementType(), device.Context, device.Queue, expr.EvalUntyped(), danglingBuffer.Dimensions, danglingBuffer.Buffer)
            // Remove the entry from dangling buffers
            this.DanglingBuffers.Remove(expr)
        else
            raise (KernelCallException("You are trying to read an invalid buffer [" + expr.ToString() + "]"))
                 
    new () = {
        compiler = new KernelCompiler()
        DanglingBuffers = new Dictionary<Expr, KernelDanglingBuffer>()
    }
    new (comp) = {
        compiler = comp
        DanglingBuffers = new Dictionary<Expr, KernelDanglingBuffer>()
    }
      
module KernelExtension =
    let ker(f:'T -> unit) =
        fun (t:'T) -> (fun(r:KernelRunner, gSize, lSize) -> r.Run(<@ f @>, gSize, lSize))

    let kernel(f:'T -> 'U) =
        fun (t:'T, globalSize: int array, localSize: int array, runner: KernelRunner) ->
            runner.Run(<@ f @>, globalSize, localSize)
  
            
            

