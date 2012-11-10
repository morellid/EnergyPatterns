namespace MetricUtil
open Microsoft.FSharp.Quotations
open System.Reflection

type ProfilingDevice<'T>(value:'T) =
    member this.Device with get() = value

type ProfilingResult<'T>(value:'T) =
    member this.Result with get() = value
    
type EvaluationResult<'T>(value:'T) =
    member this.Result with get() = value

type RelativeMetric<'T,'U,'Z> =
    abstract member Evaluate: ProfilingResult<'U> * Expr -> EvaluationResult<'Z>
    abstract member Profile: ProfilingDevice<'T> * ProfilingDevice<'T> -> ProfilingResult<'U>
    
type AbsoluteMetric<'T,'U,'Z> =
    abstract member Evaluate: ProfilingResult<'U> * Expr -> EvaluationResult<'Z>
    abstract member Profile: ProfilingDevice<'T> -> ProfilingResult<'U>
