namespace FSCL

open FSCL.Transformation
open FSCL.Transformation.Processors
open System.Reflection
open Microsoft.FSharp.Quotations

type internal KernelCompilerTools() =
    static member DefaultTransformationPipeline() =  
        let discovery = new KernelDiscoveryStage()
        let signature = new KernelSignatureTransformationStage()
        let body = new KernelBodyTransformationStage()

        discovery.DiscoveryProcessors.Add(new KernelByNameDiscoveryProcessor())

        signature.ParameterProcessors.Add(new DefaultParameterProcessor())
        signature.SignatureProcessors.Add(new DefaultSignatureProcessor())

        body.CallProcessors.Add(new ImplicitCallProcessor())
        body.CallProcessors.Add(new ArrayAccessProcessor())
        body.CallProcessors.Add(new ArithmeticOperationProcessor())

        body.IfThenElseProcessors.Add(new DefaultIfThenElseProcessor())
        body.IntegerRangeLoopProcessors.Add(new DefaultIntegerRangeLoopProcessor())
        body.WhileLoopProcessors.Add(new DefaultWhileLoopProcessor())
        body.LetProcessors.Add(new DefaultLetProcessor())
        body.SequentialProcessors.Add(new DefaultSequentialProcessor())
        body.VarProcessors.Add(new DefaultVarProcessor())
        body.VarSetProcessors.Add(new DefaultVarSetProcessor())
        body.ValueProcessors.Add(new DefaultValueProcessor())

        body.TypeProcessors.Add(new PlainTypeProcessor())
        body.TypeProcessors.Add(new ArrayTypeProcessor())

        body.UnionCaseProcessors.Add(new DefaultUnionCaseProcessor())

        body.PropertyGetProcessors.Add(new MacroProcessor())

        body.GenericProcessors.Add(new ConditionalAssignmentProcessor())

        // Run pipeline
        ((discovery --> signature) + signature) --> body
        
    // Kernel extraction tools
    static member GetKernelArrayDimensions (t:System.Type) =
        // If not array return 0
        if t.IsArray then
            // Any better way to do this?
            let dimensionsString = t.FullName.Split([| '['; ']' |]).[1]
            let dimensions = ref 1
            String.iter (fun c -> if (c = ',') then dimensions := !dimensions + 1) dimensionsString
            !dimensions
        else
            0

    static member GetKernelArrayLength (o) =
        if o.GetType().IsArray then
            Some(o.GetType().GetProperty("Length").GetValue(o) :?> int)
        else
            None
            
    static member GetKernelAdditionalParameters(t:System.Type) =
        // If not array return 0
        if t.IsArray then
            // Any better way to do this?
            let dimensionsString = t.FullName.Split([| '['; ']' |]).[1]
            let dimensions = ref 1
            String.iter (fun c -> if (c = ',') then dimensions := !dimensions + 1) dimensionsString
            !dimensions
        else
            0
        
    // Extract method info from kernel name or kernel call
    static member private IsKernelCall(expr: Expr) =
        match expr with
        | Patterns.Call (e, i, a) ->
            match i with
            | DerivedPatterns.MethodWithReflectedDefinition(b) -> 
                true
            | _ ->
                false
        | _ ->
            false
        
    static member ExtractMethodInfo (expr:Expr) =
        let isKernelCall = KernelCompilerTools.IsKernelCall(expr)
        let rec ExtractMethodInfoInner (expr) = 
            match expr with
            | Patterns.Lambda(v, e) -> 
                ExtractMethodInfoInner (e)
            | Patterns.Let (v, e1, e2) ->
                ExtractMethodInfoInner (e2)
            | Patterns.Call (e, i, a) ->
                match i with
                | DerivedPatterns.MethodWithReflectedDefinition(b) ->                    
                    (isKernelCall, i, Array.mapi (fun i (p:ParameterInfo) -> (p, KernelCompilerTools.GetKernelArrayDimensions(p.ParameterType), a.[i])) (i.GetParameters()))
                | _ ->
                    raise (KernelTransformationException("A kernel definition must provide a function marked with ReflectedDefinition attribute"))
            | _-> 
                raise (KernelTransformationException("Cannot find a kernel function definition inside the expression"))
        
        ExtractMethodInfoInner(expr)


