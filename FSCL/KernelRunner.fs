namespace FSCL

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection

type KernelAttributeException(msg: string) =
    inherit System.Exception(msg)

type KernelRunner() =
    static member Init() =
        // Find out kernels in the calling assembly
        let assembly = Assembly.GetCallingAssembly()
        let types = (assembly.GetTypes()) 
        let kernels = seq {
                        for t in types do
                            let methods = t.GetMethods()
                            for meth in methods do
                                let attrs = meth.CustomAttributes
                                let containsAttr = (Seq.tryFind(fun (attr:CustomAttributeData) -> attr.AttributeType = typeof<KernelAttribute>) attrs)
                                if containsAttr.IsSome then
                                    yield meth
                           }
        // For each kernel analyze, create device, translate it into CL and compile
        Seq.iter (fun (kernel:MethodInfo) ->
            let mutable platformIndex = 0
            let mutable deviceIndex = 0

            // Check if a particular device is specified by the user via KernelAttribute
            let kernelAttribute = kernel.GetCustomAttribute<KernelAttribute>()
            if kernelAttribute.Device >= 0 && kernelAttribute.Platform >= 0 then
                // Check if platform and device indexes are valid
                if ComputePlatform.Platforms.Count <= platformIndex || (ComputePlatform.Platforms.[platformIndex]).Devices.Count <= deviceIndex then
                    raise (new KernelAttributeException("The platform and device indexes specified for the kernel " + kernel.Name + " are invalid"))
                
                platformIndex <- kernelAttribute.Platform
                deviceIndex <- kernelAttribute.Device
            else
                // The heart: find best device using a metric (by now fixed assignment)
                platformIndex <- 0
                deviceIndex <- 0

            // Get method body as expr thanks to reflected definition
            let kernelBody = 
                match kernel with
                | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                    b
                | _ ->
                    raise (new KernelAttributeException("The kernel " + kernel.Name + " must be labeled with ReflectedDefinition attribute to be recognized"))
                
            // Translate to cl and build program and kernel object            
            let platform = ComputePlatform.Platforms.[platformIndex]
            let device = platform.Devices.[deviceIndex]
            let contextProperties = new ComputeContextPropertyList(platform)
            let devices = new System.Collections.Generic.List<ComputeDevice>();
            devices.Add(device)
            let computeContext = new ComputeContext(devices, contextProperties, null, System.IntPtr.Zero)  
             
            // Invoke F# to CL transformation
            let conversionData = KernelBinding.ConvertToCLKernel(kernelBody)
            let (kernelSource:string, argInfo:(ParameterInfo * Expr) list, methodInfo:MethodInfo) = conversionData.Value  
            let computeProgram = new ComputeProgram(computeContext, kernelSource)
            computeProgram.Build(devices, "", null, System.IntPtr.Zero)
            let computeKernel = computeProgram.CreateKernel(methodInfo.Name)
            ())
                                           


    static member Run(expr: Expr, globalSize, localSize) =
        let kernel = KernelBinding.ConvertToCLKernel(expr)
        match kernel with
        | Some(kernelSource, args, methodInfo) ->
            ()
            // Something should be moved in global scope
            (*
            let computeQueue = new ComputeCommandQueue(computeContext, device, ComputeCommandQueueFlags.OutOfOrderExecution)
            let inputBuffer = new ComputeBuffer<float>(computeContext, ComputeMemoryFlags.ReadOnly, 4L)
            let outputBuffer = new ComputeBuffer<float>(computeContext, ComputeMemoryFlags.WriteOnly, 4L)
            computeKernel.SetMemoryArgument(0, inputBuffer)
            computeKernel.SetMemoryArgument(1, outputBuffer)
            computeQueue.WriteToBuffer([| 1.0 |], inputBuffer, true, null) 
            computeQueue.Execute(computeKernel, [| 0L |], globalSize, localSize, null) 
            computeQueue.Finish()
            *)
        | _ ->
            ()

            
            
            
            

