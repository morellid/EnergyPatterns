namespace FSCL.Compiler.Processors

open FSCL.Compiler
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations

type KernelReferenceParser() =          
    let rec LiftArgExtraction (expr, parameters: ParameterInfo[], vars:List<Var>) =
        match expr with
        | Patterns.Lambda(v, e) ->
            LiftArgExtraction (e, parameters, vars)
        | Patterns.Let(v, value, body) ->
            let el = Array.tryFind (fun (p:ParameterInfo) -> p.Name = v.Name) parameters
            if el.IsSome then
                vars.Add(v)
                LiftArgExtraction (body, parameters, vars)
            else
                expr
        | _ ->
            expr

    let rec GetKernelFromName(expr, k:ParserStep<Expr>) =                    
        match expr with
        | Patterns.Lambda(v, e) -> 
            GetKernelFromName (e, k)
        | Patterns.Let (v, e1, e2) ->
            GetKernelFromName (e2, k)
        | Patterns.Call (e, i, a) ->
            match i with
            | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                let kernelAttribute = i.GetCustomAttribute<FSCL.KernelAttribute>()  
                if kernelAttribute.Device >= 0 && kernelAttribute.Platform >= 0 then                       
                    k.AddCompilerData("KERNEL_PLATFORM", kernelAttribute.Platform.ToString())   
                    k.AddCompilerData("KERNEL_DEVICE", kernelAttribute.Device.ToString()) 
                let paramVarList = new List<Var>() 
                let cleanBody = LiftArgExtraction(b, i.GetParameters(), paramVarList)
                // Store param var list into global data
                k.AddCompilerData("KERNEL_PARAMETER_PLACEHOLDER", paramVarList)
                Some(i, cleanBody)
            | _ ->
                None//raise (KernelTransformationException("A kernel definition must provide a function marked with ReflectedDefinition attribute"))
        | _-> 
            None//raise (KernelTransformationException("Cannot find a kernel function definition inside the expression"))

        
    interface ParserProcessor<Expr> with
        member this.Handle(expr, engine:ParserStep<Expr>) =
            let ker = GetKernelFromName(expr, engine)
            ker
            