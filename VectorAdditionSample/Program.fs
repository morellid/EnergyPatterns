// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FSCL
open InstructionEnergyMetric
open MetricUtil
open Cloo

[<Kernel>]
[<ReflectedDefinition>]
let VectorAdd(a: float[], b: float[], c: float[]) =
    let gid = fscl.get_global_id(0)
    for i = 0 to a.Length do
        printf ""
    c.[gid] <- a.[gid] + b.[gid]

[<EntryPoint>]
let main argv =
    let metric = new InstructionEnergyMetric()
    metric.MinInstr <- 1
    metric.MaxInstr <- 2048
    metric.Step <- 100
    metric.PerStepDuration <- 20000
    metric.ThreadCount <- 2048L

    (metric :> AbsoluteMetric<ComputeDevice, EnergyFunction, double>).Evaluate([], System.Reflection.MethodBase.
    // Testing vector types
    (*
    let t = new FSCL.OpenCLVector4D()
    t.s0
    t.xw <- [| 0.; 1. |]
    t.s1230 <- [| 1.;2.;3.;4. |]
    let g = t.even
    let g2 = t.odd
    printfn "%A" argv
    *)
    0 // return an integer exit code
