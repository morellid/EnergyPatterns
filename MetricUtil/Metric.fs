namespace MetricBase

open Microsoft.FSharp.Quotations

type ProfilingDevice<'T>(value:'T) =
    member this.Device with get() = value

type ProfilingResult<'T>(value:'T) =
    member this.Result with get() = value

type InstantiationResult<'T>(value:'T) =
    member this.Result with get() = value
    
type Metric<'U,'Z> =
    abstract member Evaluate: ProfilingResult<'U> * Expr -> Expr
    abstract member Instantiate: Expr * Expr -> InstantiationResult<'Z>

type RelativeMetric<'T,'U,'Z> =
    inherit Metric<'U,'Z>
    abstract member Profile: ProfilingDevice<'T> * ProfilingDevice<'T> -> ProfilingResult<'U>
    
type AbsoluteMetric<'T,'U,'Z> =
    inherit Metric<'U,'Z>
    abstract member Profile: ProfilingDevice<'T> -> ProfilingResult<'U>        

