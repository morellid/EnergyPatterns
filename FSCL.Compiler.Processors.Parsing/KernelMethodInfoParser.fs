namespace FSCL.Compiler.Processors

open FSCL.Compiler
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations

type KernelMethodInfoParser() =          
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

    let rec GetKernelFromName(mi, k:ParserStep<MethodInfo>) =   
        match mi with
        | DerivedPatterns.MethodWithReflectedDefinition(b) ->
            let kernelAttribute = mi.GetCustomAttribute<FSCL.KernelAttribute>()  
            if kernelAttribute.Device >= 0 && kernelAttribute.Platform >= 0 then                       
                k.AddCompilerData("KERNEL_PLATFORM", kernelAttribute.Platform.ToString())   
                k.AddCompilerData("KERNEL_DEVICE", kernelAttribute.Device.ToString()) 
            let paramVarList = new List<Var>() 
            let cleanBody = LiftArgExtraction(b, mi.GetParameters(), paramVarList)
            // Store param var list into global data
            k.AddCompilerData("KERNEL_PARAMETER_PLACEHOLDER", paramVarList)
            Some(mi, cleanBody)
        | _ ->
            None//raise (KernelTransformationException("A kernel definition must provide a function marked with ReflectedDefinition attribute"))
       
        
    interface ParserProcessor<MethodInfo> with
        member this.Handle(mi, engine:ParserStep<MethodInfo>) =
            let ker = GetKernelFromName(mi, engine)
            ker
            