namespace FSCL.Compiler.Processors

open FSCL.Compiler
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations
open System

type SignaturePrinter() =        
    let rec LiftArgExtraction (expr, parameters: Reflection.ParameterInfo[]) =
        match expr with
        | Patterns.Lambda(v, e) ->
            LiftArgExtraction (e, parameters)
        | Patterns.Let(v, value, body) ->
            let el = Array.tryFind (fun (p : Reflection.ParameterInfo) -> p.Name = v.Name) parameters
            if el.IsSome then
                LiftArgExtraction (body, parameters)
            else
                expr
        | _ ->
            expr
            
    interface SignaturePrettyPrinterProcessor with
        member this.Handle(signature, engine:PrettyPrinterStep) =
            // Convert params and produce additional params
            let kernelParams = signature.GetParameters()
            let kernelParamsInfo = engine.CompilerData("KERNEL_PARAMETER_TABLE").Value :?> KernelParameterTable
            // Create KERNEL_PARAMETER_TABLE
            let paramsPrint = Array.map(fun (p:ParameterInfo) ->
                if p.ParameterType.IsArray then
                    // If the parameters is tagged with Contant attribute, prepend constant keyword, else global
                    let addressSpace = kernelParamsInfo.[p.Name].AddressSpace
                    if addressSpace = KernelParameterAddressSpace.LocalSpace then
                        "local " + engine.Process(p.ParameterType) + p.Name
                    elif addressSpace = KernelParameterAddressSpace.ConstantSpace then
                        "constant " + engine.Process(p.ParameterType) + p.Name
                    else
                        "global " + engine.Process(p.ParameterType) + p.Name
                 else
                    engine.Process(p.ParameterType) + " " + p.Name) kernelParams
            
            let signature = Some("kernel " + signature.Name + "(" + (String.concat ", " paramsPrint) + ")")
            signature

           