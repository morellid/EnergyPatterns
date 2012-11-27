namespace MetricBase

open Microsoft.FSharp.Quotations

type MetricBase() =
    let mutable (children:MetricBase list) = []
    member this.SubMetric with get() = children and set v = children <- v
       
[<AbstractClass>]
type Metric<'T,'U,'Z,'CDATA>() =
    inherit MetricBase()
    abstract member Evaluate: 'T * Expr -> 'U
    abstract member Instantiate: 'T * 'U * Expr * 'CDATA -> 'Z
    
[<AbstractClass>]
type RelativeMetric<'T,'U,'Z,'W,'CDATA>() =
    inherit Metric<'U,'Z,'W,'CDATA>()
    abstract member Profile: 'T list -> 'U
    
[<AbstractClass>]
type AbsoluteMetric<'T,'U,'Z,'W,'CDATA>() =
    inherit Metric<'U,'Z,'W,'CDATA>()
    abstract member Profile: 'T -> 'U       

