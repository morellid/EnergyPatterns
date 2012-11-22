namespace MetricBase

open Microsoft.FSharp.Quotations
    
type Metric<'T,'U,'Z> =
    abstract member Evaluate: 'T * Expr -> 'U
    abstract member Instantiate: 'T * 'U * Expr -> 'Z

type RelativeMetric<'T,'U,'Z,'W> =
    inherit Metric<'U,'Z,'W>
    abstract member Profile: 'T list -> 'U
    
type AbsoluteMetric<'T,'U,'Z,'W> =
    inherit Metric<'U,'Z,'W>
    abstract member Profile: 'T -> 'U       

