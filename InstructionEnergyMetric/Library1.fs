namespace InstructionEnergyMetric

open MetricUtil
open Cloo
open Microsoft.FSharp.Quotations

type EnergyFunction = (int * double) list

type InstructionEnergyMetric() =
    let mutable min_instr = 1
    let mutable max_instr = 1
    let mutable step = 1
    let mutable per_step_duration = 0

    member this.MinInstr 
        with get() = min_instr
        and set instr = min_instr <- instr

    member this.MaxInstr 
        with get() = max_instr
        and set instr = max_instr <- instr
        
    member this.Step 
        with get() = step
        and set instr = step <- instr

    member this.PerStepDuration 
        with get() = per_step_duration
        and set duration = per_step_duration <- duration

    interface AbsoluteMetric<ComputeDevice, EnergyFunction, double> with
        member this.Profile(device) =
            // Setup CL
            let computePlatform = device.Device.Platform;
            let contextProperties = new ComputeContextPropertyList(computePlatform)
            let devices = new System.Collections.Generic.List<ComputeDevice>();
            devices.Add(device.Device)
            let computeContext = new ComputeContext(devices, contextProperties, null, System.IntPtr.Zero);
            
            // Calculate list of instr count
            let instrCount = seq { 
                                    let i = ref this.MinInstr
                                    while !i <= this.MaxInstr do 
                                        yield !i
                                        i := !i + this.Step
                                 }

            // For each instr count run the test
            for currInstr in instrCount do
                let computeProgram = new ComputeProgram(computeContext, KernelBuilder.BuildKernel(currInstr))
                computeProgram.Build(devices, "", null, System.IntPtr.Zero)
                let computeKernel = computeProgram.CreateKernel("run")
                let computeQueue = new ComputeCommandQueue(computeContext, computeDevice, ComputeCommandQueueFlags.OutOfOrderExecution)

                // Run kernel n times to guarantee a total time >= PerStepDuration
                computeQueue.Execute(computeKernel, 
            new ProfilingResult<EnergyFunction>([])
        member this.Evaluate(profiling, expr) =
            new EvaluationResult<double>(0.0)

