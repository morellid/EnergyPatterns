namespace FSCL

open Cloo
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Reflection
open System.Reflection
open Microsoft.FSharp.Linq.QuotationEvaluation
open FSCL.Compiler.Processors
open FSCL.Compiler
        
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

type KernelCompiler =   
    val globalDataStorage : FSCLGlobalData
    val embeddedMetric : MetricBase.MetricBase option
    val transformationPipeline : CompilerStep<Expr option * MethodInfo option, string>
     
    // Utility function to store kernels found all around the assembly. Called by the constructor
    member private this.StoreKernel(globalData:FSCLGlobalData, kernel:MethodInfo, platformIndex, deviceIndex) =    
        // Get method body as expr thanks to reflected definition and fail if no reflected method found
        let kernelBody = 
            match kernel with
            | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                b
            | _ ->
                raise (new KernelDefinitionException("The kernel " + kernel.Name + " must be labeled with ReflectedDefinition attribute to be recognized"))
       
        // Convert kernel         
        let conversionData = this.transformationPipeline.Run(None, Some(kernel))
        let state = this.transformationPipeline.CompilerDataCopy

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
       
    member private this.AnalyzeAndStoreKernel(kernel:MethodInfo) =
        // For each kernel analyze, create device, translate it into CL and compile
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
            this.StoreKernel(this.GlobalDataStorage, kernel, platformIndex, deviceIndex)
        // No statically determined device: build kernel for all the possible devices
        else
            // The heart: find best device using a metric (by now fixed assignment)
            platformIndex <- 0
            deviceIndex <- 0    
                
            for platform in ComputePlatform.Platforms do
                for device in platform.Devices do
                    this.StoreKernel(this.GlobalDataStorage, kernel, platformIndex, deviceIndex)
        // Return the kernel (not the instance!)
        this.GlobalDataStorage.Kernels.[this.GlobalDataStorage.Kernels.Length - 1]

    member private this.AnalyzeAndStoreKernel(kernel:Expr) =
        let (c, mi, args) = KernelCompilerTools.ExtractMethodInfo(kernel)
        this.AnalyzeAndStoreKernel(mi)

    member private this.DiscoverKernels() =
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
        kernels
    
    // Properties   
    member this.GlobalDataStorage 
        with get() =
            this.globalDataStorage

    member this.EmbeddedMetric
        with get() =
            this.embeddedMetric
            
    member this.TransformationPipeline
        with get() =
            this.transformationPipeline

    // Methods
    member this.Discover () =
        let kernels = this.DiscoverKernels()        
        for kernel in kernels do
            this.AnalyzeAndStoreKernel(kernel) |> ignore

    member this.Add (kernel:MethodInfo) =  
        this.AnalyzeAndStoreKernel(kernel)
        
    member this.Add (kernel:Expr) =  
        this.AnalyzeAndStoreKernel(kernel)

    // Constructors
    new (metric, pipeline) = {
        globalDataStorage = new FSCLGlobalData()
        embeddedMetric = Some(metric)
        transformationPipeline = pipeline
    }
    new (metric) = {
        globalDataStorage = new FSCLGlobalData()
        embeddedMetric = Some(metric)
        transformationPipeline = KernelCompilerTools.DefaultTransformationPipeline()
    }       
    new (pipeline) = {
        globalDataStorage = new FSCLGlobalData()
        embeddedMetric = None
        transformationPipeline = pipeline
    }     
    new () = {
        globalDataStorage = new FSCLGlobalData()
        embeddedMetric = None
        transformationPipeline = KernelCompilerTools.DefaultTransformationPipeline()
    }