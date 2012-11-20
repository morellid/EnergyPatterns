namespace FSCL

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open Microsoft.FSharp.Linq.QuotationEvaluation
        
type FSCLDeviceData(device:ComputeDevice, context, queue) =
    member val Device = device with get
    member val Context = context with get
    member val Queue = queue with get
    
type FSCLCompiledKernelData(program, kernel, device) =
    member val Program = program with get 
    member val Kernel = kernel with get
    member val DeviceIndex = device with get

type FSCLKernelData(kernel) =
    member val MethodInfo = kernel with get
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
       
        // Discover platform and device
        let platform = ComputePlatform.Platforms.[platformIndex]
        let device = platform.Devices.[deviceIndex]   
        let devices = new System.Collections.Generic.List<ComputeDevice>();
        devices.Add(device)
        
        // Check if kernel already stored
        let mutable kernelIndex = List.tryFindIndex(fun (k:FSCLKernelData) -> k.MethodInfo = kernel) globalData.Kernels
        if kernelIndex.IsNone then
            // Store kernel
            globalData.Kernels <- globalData.Kernels @ [ new FSCLKernelData(kernel) ]
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
        let conversionData = KernelBinding.ConvertToCLKernel(kernel)
        let (kernelSource:string, argInfo:ParameterInfo[]) = conversionData.Value  
        let computeProgram = new ComputeProgram(globalData.Devices.[deviceIndex.Value].Context, kernelSource)
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

    // Run a kernel through a quoted kernel call
    member this.Run(expr: Expr, size: (int * int) list) =
        let (kernelInfo, args) = FSCL.Util.GetKernelFromCall (expr)

        // Found a kernel in global data matching the call
        let matchingKernel = List.tryFind(fun (k:FSCLKernelData) -> k.MethodInfo = kernelInfo) (fsclData.Kernels)
        if matchingKernel.IsNone then
            raise (new KernelCallException("No kernel named " + kernelInfo.Name + " can be found"))
            
        // Fix: here to be called INSTANTIATE on a metric to get the device to use
        let kernelInstance = matchingKernel.Value.Instances.[0]
        let queue = fsclData.Devices.[kernelInstance.DeviceIndex].Queue
        let context = fsclData.Devices.[kernelInstance.DeviceIndex].Context
        ()
        // FIX: determine best read/write strategy

        // For each parameter, create buffer (if array), write it and set kernel arg
        (*
        List.iteri (fun index (par:ParameterInfo, arg:Expr) ->
            if par.ParameterType.IsArray then
                // Create buffer
                let t = par.ParameterType.GetElementType()
                let actualArg = arg.EvalUntyped()
                if (t = typeof<uint32>) then
                    let buffer = new ComputeBuffer<uint32>(context, ComputeMemoryFlags.None, actualArg)
                    
                elif (t = typeof<uint64>) then
                    "unsigned long"        
                elif (t = typeof<int64>) then
                    "long"               
                elif (t = typeof<int>) then
                    "int"            
                elif (t = typeof<double>) then
                    "double"
                elif (t = typeof<float32>) then
                    "float"
                elif (t = typeof<bool>) then
                    "bool"
                    
            else
            (*
                let genericMethod = kernelInstance.Kernel.GetType().GetMethod("SetValueArgument").GetGenericMethodDefinition()
                let specificMethod = genericMethod.MakeGenericMethod([| par.ParameterType |])
                specificMethod.Invoke(null, [| arg.EvalUntyped() |]) |> ignore) args *)
                kernelInstance.Kernel.SetValueArgumentAsObject(index, arg.EvalUntyped())) *)


            


            
            
            

