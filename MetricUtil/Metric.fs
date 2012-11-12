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
    abstract member Instantiate: Expr * Expr -> InstantiationResult<'Z>

type RelativeMetric<'T,'U,'Z> =
    inherit Metric<'U,'Z>
    abstract member Profile: ProfilingDevice<'T> * ProfilingDevice<'T> -> ProfilingResult<'U>
    
type AbsoluteMetric<'T,'U,'Z> =
    inherit Metric<'U,'Z>
    abstract member Profile: ProfilingDevice<'T> -> ProfilingResult<'U>        

module MetricTools =
    // Extracts a method with reflected definition from a quotation containing its name
    let rec ExtractKernelDefinition (expr) =
        match expr with
        | Patterns.Lambda(v, e) -> 
            ExtractKernelDefinition e
        | Patterns.Let (v, e1, e2) ->
            ExtractKernelDefinition (e2)
        | Patterns.Call (e, i, a) ->
            match i with
            | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                i
            | _ ->
                raise (MalformedKernelError("A kernel function must be marked with ReflectedDefinition attribute"))
        | _-> 
            raise (MalformedKernelError("Cannot find a kernel function inside the expression"))

    let rec ExtractKernelInvocation (expr) =
        let getArgs exp =
            match exp with
            | Patterns.Call (e, i, a) ->
                match i with
                | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                    a
                | _ ->
                    raise (MalformedKernelError("A kernel invocation must provide a function marked with ReflectedDefinition attribute"))
            | _-> 
                raise (MalformedKernelError("Cannot find a kernel function invocation inside the expression"))

        let args = getArgs expr
        let kernel = ExtractKernelDefinition expr 
        (kernel, args)
