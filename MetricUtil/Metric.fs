namespace MetricUtil
open Microsoft.FSharp.Quotations

type ProfilingDevice<'T>(value:'T) =
    member this.Device with get() = value

type ProfilingResult<'T>(value:'T) =
    member this.Result with get() = value
    
type EvaluationResult<'T>(value:'T) =
    member this.Result with get() = value

type Metric<'T> =
    abstract member Evaluate: Expr -> EvaluationResult<'T>
    
type RelativeMetric<'T,'U,'Z> =
    inherit Metric<'T> 
    abstract member Profile: ProfilingDevice<'Z> * ProfilingDevice<'Z> -> ProfilingResult<'U>
    
type AbsoluteMetric<'T,'U,'Z> =
    inherit Metric<'T> 
    abstract member Profile: ProfilingDevice<'Z> -> ProfilingResult<'U>