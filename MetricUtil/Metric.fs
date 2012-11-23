namespace MetricBase

open Microsoft.FSharp.Quotations

type MetricBase() =
    let mutable (children:MetricBase list) = []
    member this.SubMetric with get() = children and set v = children <- v
       
[<AbstractClass>]
type Metric<'T,'U,'Z>() =
    inherit MetricBase()
    abstract member Evaluate: 'T * Expr -> 'U
    abstract member Instantiate: 'T * 'U * Expr -> 'Z
    
[<AbstractClass>]
type RelativeMetric<'T,'U,'Z,'W>() =
    inherit Metric<'U,'Z,'W>()
    abstract member Profile: 'T list -> 'U
    
[<AbstractClass>]
type AbsoluteMetric<'T,'U,'Z,'W>() =
    inherit Metric<'U,'Z,'W>()
    abstract member Profile: 'T -> 'U       

