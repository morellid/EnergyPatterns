namespace FSCL

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection

type KernelAttributeException(msg: string) =
    inherit System.Exception(msg)
        
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
    static member private StoreNewKernel(globalData:FSCLGlobalData, kernel:MethodInfo, platformIndex, deviceIndex) =    
        // Get method body as expr thanks to reflected definition and fail if no reflected method found
        let kernelBody = 
            match kernel with
            | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                b
            | _ ->
                raise (new KernelAttributeException("The kernel " + kernel.Name + " must be labeled with ReflectedDefinition attribute to be recognized"))
       
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
           
        // Bind the kernel to the device storing appropriate kernel implementation                                             
        // Create and build program
        let conversionData = KernelBinding.ConvertToCLKernel(kernelBody)
        let (kernelSource:string, argInfo:(ParameterInfo * Expr) list, methodInfo:MethodInfo) = conversionData.Value  
        let computeProgram = new ComputeProgram(globalData.Devices.[deviceIndex.Value].Context, kernelSource)
        computeProgram.Build(devices, "", null, System.IntPtr.Zero)
        let computeKernel = computeProgram.CreateKernel(methodInfo.Name)

        // Add kernel implementation to the list of implementations for the given kernel
        let compiledKernel = new FSCLCompiledKernelData(computeProgram, computeKernel, deviceIndex.Value)
        kernelData.Instances <- kernelData.Instances @ [ compiledKernel ]
                
        
    static member Init() =
        let fsclData = new FSCLGlobalData()

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
                KernelRunner.StoreNewKernel(fsclData, kernel, platformIndex, deviceIndex)
            // No statically determined device: build kernel for all the possible devices
            else
                // The heart: find best device using a metric (by now fixed assignment)
                platformIndex <- 0
                deviceIndex <- 0    
                
                for platform in ComputePlatform.Platforms do
                    for device in platform.Devices do
                        KernelRunner.StoreNewKernel(fsclData, kernel, platformIndex, deviceIndex)) kernels
        fsclData

    static member Run(expr: Expr, globalData: FSCLGlobalData, globalSize, localSize) =
        let call = FSCL.Util.GetKernelCall(expr)
        call

            
            
            
            

