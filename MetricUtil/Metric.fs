namespace MetricUtil

open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Core.CompilerServices
open System.Diagnostics

type ProfilingDevice<'T>(value:'T) =
    member this.Device with get() = value

type ProfilingResult<'T>(value:'T) =
    member this.Result with get() = value

type InstantiationResult<'T>(value:'T) =
    member this.Result with get() = value
    
type Metric<'U,'Z> =
    abstract member Evaluate: ProfilingResult<'U> * Expr -> Expr
    abstract member Instantiate: Expr * obj list -> InstantiationResult<'Z>

type RelativeMetric<'T,'U,'Z> =
    inherit Metric<'U,'Z>
    abstract member Profile: ProfilingDevice<'T> * ProfilingDevice<'T> -> ProfilingResult<'U>
    
type AbsoluteMetric<'T,'U,'Z> =
    inherit Metric<'U,'Z>
    abstract member Profile: ProfilingDevice<'T> -> ProfilingResult<'U>        

module MetricTools =
    // Extracts a method with reflected definition from a quotation containing its name
    let rec ExtractKernel (expr) =
        match expr with
        | Patterns.Lambda(v, e) -> 
            ExtractKernel e
        | Patterns.Let (v, e1, e2) ->
            ExtractKernel (e2)
        | Patterns.Call (e, i, a) ->
            match i with
            | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                Some(i)
            | _ ->
                None
        | _-> 
            None
