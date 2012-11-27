namespace FSCL

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open Microsoft.FSharp.Linq.QuotationEvaluation
open FSCL.Transformation.Processors
open FSCL.Transformation
        
type FSCLDeviceData(device:ComputeDevice, context, queue) =
    member val Device = device with get
    member val Context = context with get
    member val Queue = queue with get
    
type FSCLCompiledKernelData(program, kernel, device) =
    member val Program = program with get 
    member val Kernel = kernel with get
    member val DeviceIndex = device with get

type FSCLKernelData(kernel, parameters) =
    member val MethodInfo = kernel with get
    member val Parameters = parameters with get
    // List of devices and kernel instances potentially executing the kernel
    member val Instances:FSCLCompiledKernelData list = [] with get, set       

type FSCLGlobalData() =
    member val Kernels:FSCLKernelData list = [] with get, set
    member val Devices:FSCLDeviceData list = [] with get, set

type KernelRunner() =
    let fsclData = new FSCLGlobalData()

    // Utility function to store kernels found all around the assembly. Called by the constructor
    let StoreNewKernel(globalData:FSCLGlobalData, kernel:MethodInfo, platformIndex, deviceIndex) =    
        // Get method body as expr thanks to reflected definition and fail if no reflected method found
        let kernelBody = 
            match kernel with
            | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                b
            | _ ->
                raise (new KernelDefinitionException("The kernel " + kernel.Name + " must be labeled with ReflectedDefinition attribute to be recognized"))
       
        // Convert kernel         
        let (conversionData, state) = KernelBinding.Compile(kernel)

        // Discover platform and device
        let platform = ComputePlatform.Platforms.[platformIndex]
        let device = platform.Devices.[deviceIndex]   
        let devices = new System.Collections.Generic.List<ComputeDevice>();
        devices.Add(device)
        
        // Check if kernel already stored
        let mutable kernelIndex = List.tryFindIndex(fun (k:FSCLKernelData) -> k.MethodInfo = kernel) globalData.Kernels
        if kernelIndex.IsNone then
            // Store kernel
            let argInfo = state.["KERNEL_PARAMETER_TABLE"] :?> KernelParameterTable
            globalData.Kernels <- globalData.Kernels @ [ new FSCLKernelData(kernel, argInfo) ]
            kernelIndex <- Some(globalData.Kernels.Length - 1)
        let kernelData = globalData.Kernels.[kernelIndex.Value]

        // Check if device already stored
        let mutable deviceIndex = List.tryFindIndex (fun (dev:FSCLDeviceData) -> dev.Device.Handle = device.Handle) globalData.Devices
        if deviceIndex.IsNone then
            // Store device, context and queue (one per device)
            let contextProperties = new ComputeContextPropertyList(platform)
            let computeContext = new ComputeContext(devices, contextProperties, null, System.IntPtr.Zero) 
            let computeQueue = new ComputeCommandQueue(computeContext, device, ComputeCommandQueueFlags.None) 
            // Add device to the list of global devices
            deviceIndex <- Some(globalData.Devices.Length)
            let deviceData = new FSCLDeviceData(device, computeContext, computeQueue)
            globalData.Devices <- globalData.Devices @ [ deviceData ]
            deviceIndex <- Some(globalData.Kernels.Length - 1)
           
        // Bind the kernel to the device storing appropriate kernel implementation                                             
        // Create and build program
        let computeProgram = new ComputeProgram(globalData.Devices.[deviceIndex.Value].Context, conversionData)
        try
            computeProgram.Build(devices, "", null, System.IntPtr.Zero)
        with
        | ex -> 
            let log = computeProgram.GetBuildLog(device)
            raise (new KernelDefinitionException("Kernel build fail: " + log))
        
        let computeKernel = computeProgram.CreateKernel(kernel.Name)

        // Add kernel implementation to the list of implementations for the given kernel
        let compiledKernel = new FSCLCompiledKernelData(computeProgram, computeKernel, deviceIndex.Value)
        kernelData.Instances <- kernelData.Instances @ [ compiledKernel ]
            
    let Init() =
        // Find out kernels in the calling assembly
        let assembly = Assembly.GetEntryAssembly()
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
        for kernel in kernels do
            let mutable platformIndex = 0
            let mutable deviceIndex = 0

            // Check if a particular device is specified by the user via KernelAttribute
            let kernelAttribute = kernel.GetCustomAttribute<KernelAttribute>()
            if kernelAttribute.Device >= 0 && kernelAttribute.Platform >= 0 then
                // Check if platform and device indexes are valid
                if ComputePlatform.Platforms.Count <= platformIndex || (ComputePlatform.Platforms.[platformIndex]).Devices.Count <= deviceIndex then
                    raise (new KernelDefinitionException("The platform and device indexes specified for the kernel " + kernel.Name + " are invalid"))
                
                platformIndex <- kernelAttribute.Platform
                deviceIndex <- kernelAttribute.Device      
                StoreNewKernel(fsclData, kernel, platformIndex, deviceIndex)
            // No statically determined device: build kernel for all the possible devices
            else
                // The heart: find best device using a metric (by now fixed assignment)
                platformIndex <- 0
                deviceIndex <- 0    
                
                for platform in ComputePlatform.Platforms do
                    for device in platform.Devices do
                        StoreNewKernel(fsclData, kernel, platformIndex, deviceIndex)

    do Init()
    
    member private this.WriteBuffer<'T when 'T: struct>(c:ComputeContext, q:ComputeCommandQueue, arg:obj, dims, shouldInit) =
        //let dims = FSCL.Util.GetArrayDimensions(arg.Type)
        match dims with
        | 1 ->
            let actualArg = arg :?> 'T[]
            let buffer = new ComputeBuffer<'T>(c, ComputeMemoryFlags.None, actualArg.LongLength)
            if shouldInit then
                q.WriteToBuffer<'T>(actualArg, buffer, false, null)
            buffer :> ComputeMemory
        | 2 ->
            let actualArg = arg :?> 'T[,]
            let buffer = new ComputeBuffer<'T>(c, ComputeMemoryFlags.None, actualArg.LongLength)
            if shouldInit then
                let offset = Cloo.SysIntX2(0, 0)                
                let region = Cloo.SysIntX2(actualArg.GetLength(0), actualArg.GetLength(1))
                q.WriteToBuffer<'T>(actualArg, buffer, false, offset, offset, region, null)
            buffer :> ComputeMemory
        | _ ->
            let actualArg = arg :?> 'T[,,]
            let buffer = new ComputeBuffer<'T>(c, ComputeMemoryFlags.None, actualArg.LongLength)
            if shouldInit then
                let offset = Cloo.SysIntX3(0, 0, 0)
                let region = Cloo.SysIntX3(actualArg.GetLength(0), actualArg.GetLength(1), actualArg.GetLength(2)) 
                q.WriteToBuffer<'T>(actualArg, buffer, false, offset, offset, region, null)
            buffer :> ComputeMemory
            
    member private this.ReadBuffer<'T when 'T: struct>(c:ComputeContext, q:ComputeCommandQueue, arg:obj, dims, buffer: ComputeBuffer<'T>) =
        //let dims = FSCL.Util.GetArrayDimensions(arg.Type)
        match dims with
        | 1 ->
            let actualArg = arg :?> 'T[]
            q.ReadFromBuffer<'T>(buffer, ref actualArg, true, null)            
        | 2 ->
            let actualArg = arg :?> 'T[,]
            let offset = Cloo.SysIntX2(0,0)
            let region = Cloo.SysIntX2(actualArg.GetLength(0),actualArg.GetLength(1))
            q.ReadFromBuffer<'T>(buffer, ref actualArg, true, offset, offset, region, null)
        | _ ->
            let actualArg = arg :?> 'T[,,]
            let offset = Cloo.SysIntX3(0,0,0)
            let region = Cloo.SysIntX3(actualArg.GetLength(0), actualArg.GetLength(1), actualArg.GetLength(2))
            q.ReadFromBuffer<'T>(buffer, ref actualArg, true, offset, offset, region, null)
        
    // Run a kernel through a quoted kernel call
    member this.Run(expr: Expr, globalSize: int array, localSize: int array) =
             
        let (kernelInfo, args) = FSCL.Util.GetKernelFromCall (expr)

        // Found a kernel in global data matching the call
        let matchingKernel = List.tryFind(fun (k:FSCLKernelData) -> k.MethodInfo = kernelInfo) (fsclData.Kernels)
        if matchingKernel.IsNone then
            raise (new KernelCallException("No kernel named " + kernelInfo.Name + " can be found"))
            
        // Fix: here to be called INSTANTIATE on a metric to get the device to use
        let kernelInstance = matchingKernel.Value.Instances.[0]
        let queue = fsclData.Devices.[kernelInstance.DeviceIndex].Queue
        let context = fsclData.Devices.[kernelInstance.DeviceIndex].Context
        // FIX: determine best read/write strategy

        // For each parameter, create buffer (if array), write it and set kernel arg   
        let additionalArgCount = ref 0     
        let paramObjectBufferMap = new System.Collections.Generic.Dictionary<string, (System.Object * ComputeMemory)>()

        let argIndex = ref 0
        Array.iter (fun (par:ParameterInfo, dim:int, arg:Expr) ->
            if par.ParameterType.IsArray then
                let o = arg.EvalUntyped()
                // Check if constant buffer. In this case we pass the dimension (sizeof) the array and not a real buffer
                if matchingKernel.Value.Parameters.[par].AddressSpace = KernelParameterAddressSpace.LocalSpace then
                    let size = (o.GetType().GetProperty("LongLength").GetValue(o) :?> int64) * 
                               (int64 (System.Runtime.InteropServices.Marshal.SizeOf(o.GetType().GetElementType())))
                    // Set kernel arg
                    kernelInstance.Kernel.SetLocalArgument(!argIndex, size) 
                else
                    // Check if read or read_write mode
                    let matchingParameter = matchingKernel.Value.Parameters.[par]
                    let access = matchingParameter.Access
                    let mustInitBuffer =
                        ((matchingParameter.AddressSpace = KernelParameterAddressSpace.GlobalSpace) ||
                         (matchingParameter.AddressSpace = KernelParameterAddressSpace.ConstantSpace)) &&
                        ((access = KernelParameterAccessMode.ReadOnly) || 
                         (access = KernelParameterAccessMode.ReadWrite))

                    // Create buffer and eventually init it
                    let t = par.ParameterType.GetElementType()
                    let mutable buffer = None
                    if (t = typeof<uint32>) then
                        buffer <- Some(this.WriteBuffer<uint32>(context, queue, o, dim, mustInitBuffer))
                    elif (t = typeof<uint64>) then
                        buffer <- Some(this.WriteBuffer<uint64>(context, queue, o, dim ,mustInitBuffer))
                    elif (t = typeof<int64>) then
                        buffer <- Some(this.WriteBuffer<int64>(context, queue, o, dim, mustInitBuffer))
                    elif (t = typeof<int>) then
                        buffer <- Some(this.WriteBuffer<int>(context, queue, o, dim, mustInitBuffer))
                    elif (t = typeof<double>) then
                        buffer <- Some(this.WriteBuffer<double>(context, queue, o, dim, mustInitBuffer))
                    elif (t = typeof<float32>) then
                        buffer <- Some(this.WriteBuffer<float32>(context, queue, o, dim, mustInitBuffer))
                    elif (t = typeof<bool>) then
                        buffer <- Some(this.WriteBuffer<int>(context, queue, o, dim, mustInitBuffer))
                 
                    // Stor association between parameter, array and buffer object
                    paramObjectBufferMap.Add(par.Name, (o, buffer.Value))

                    // Set kernel arg
                    kernelInstance.Kernel.SetMemoryArgument(!argIndex, buffer.Value)  

                // Set additional args for array params (dimensions) 
                for dimension = 0 to dim - 1 do
                    let sizeOfDim = o.GetType().GetMethod("GetLength").Invoke(o, [| dimension |]) :?> int
                    kernelInstance.Kernel.SetValueArgument<int>(args.Length + !additionalArgCount + dimension, sizeOfDim)
                additionalArgCount := !additionalArgCount + dim
            else
                let t = par.ParameterType
                if (t = typeof<uint32>) then
                    kernelInstance.Kernel.SetValueArgument<uint32>(!argIndex, arg.EvalUntyped() :?> uint32)
                elif (t = typeof<uint64>) then
                    kernelInstance.Kernel.SetValueArgument<uint64>(!argIndex, arg.EvalUntyped() :?> uint64)
                elif (t = typeof<int64>) then
                    kernelInstance.Kernel.SetValueArgument<int64>(!argIndex, arg.EvalUntyped() :?> int64)
                elif (t = typeof<int>) then
                    kernelInstance.Kernel.SetValueArgument<int>(!argIndex, arg.EvalUntyped() :?> int)
                elif (t = typeof<double>) then
                    kernelInstance.Kernel.SetValueArgument<double>(!argIndex, arg.EvalUntyped() :?> double)
                elif (t = typeof<float32>) then
                    kernelInstance.Kernel.SetValueArgument<float32>(!argIndex, arg.EvalUntyped() :?> float32)
                elif (t = typeof<bool>) then
                    kernelInstance.Kernel.SetValueArgument<bool>(!argIndex, arg.EvalUntyped() :?> bool)
            
            argIndex := !argIndex + 1) (args)

        // Run kernel
        let offset = Array.zeroCreate<int64>(globalSize.Length)
        // 32 bit enought for size_t. Kernel uses size_t like int withour cast. We cannot put case into F# kernels each time the user does operations with get_global_id and similar!
        queue.Execute(kernelInstance.Kernel, offset, Array.map(fun el -> int64(el)) globalSize, Array.map(fun el -> int64(el)) localSize, null)

        // Read result if needed
        Array.iteri (fun index (par:ParameterInfo, dim:int, arg:Expr) ->
            if par.ParameterType.IsArray then
                if matchingKernel.Value.Parameters.[par].AddressSpace <> KernelParameterAddressSpace.LocalSpace then
                    // Get association between parameter, array and buffer object
                    let (o, buffer) = paramObjectBufferMap.[par.Name]

                    // Check if write or read_write mode
                    let mutable mustReadBuffer = false
                    let matchingParameter = matchingKernel.Value.Parameters.[par]
                    let access = matchingParameter.Access
                    mustReadBuffer <-                     
                        ((matchingParameter.AddressSpace = KernelParameterAddressSpace.GlobalSpace)) &&
                        ((access = KernelParameterAccessMode.WriteOnly) || 
                         (access = KernelParameterAccessMode.ReadWrite))

                    if(mustReadBuffer) then
                        // Create buffer and eventually init it
                        let t = par.ParameterType.GetElementType()
                        if (t = typeof<uint32>) then
                            this.ReadBuffer<uint32>(context, queue, o, dim, buffer :?> ComputeBuffer<uint32>) 
                        elif (t = typeof<uint64>) then
                            this.ReadBuffer<uint64>(context, queue, o, dim, buffer :?> ComputeBuffer<uint64>) 
                        elif (t = typeof<int64>) then
                            this.ReadBuffer<int64>(context, queue, o, dim, buffer :?> ComputeBuffer<int64>) 
                        elif (t = typeof<int>) then
                            this.ReadBuffer<int>(context, queue, o, dim, buffer :?> ComputeBuffer<int>) 
                        elif (t = typeof<double>) then
                            this.ReadBuffer<double>(context, queue, o, dim, buffer :?> ComputeBuffer<double>) 
                        elif (t = typeof<float32>) then
                            this.ReadBuffer<float32>(context, queue, o, dim, buffer :?> ComputeBuffer<float32>) 
                        elif (t = typeof<bool>) then
                            this.ReadBuffer<bool>(context, queue, o, dim, buffer :?> ComputeBuffer<bool>)) args 
                 

            
            
            

