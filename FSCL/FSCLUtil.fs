namespace FSCL

open Microsoft.FSharp.Quotations

type KernelInvocationException(msg: string) =
    inherit System.Exception(msg)
    
module Util =

    let GetKernelCall expr =
        let call =
            match expr with
            | Patterns.Call (e, i, a) ->
                match i with
                | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                    let parms = Array.zip (i.GetParameters()) (List.toArray a)
                    (i, parms)
                | _ ->
                    raise (KernelInvocationException("A kernel invocation must provide a function marked with ReflectedDefinition attribute"))
            | _-> 
                raise (KernelInvocationException("Cannot find a kernel function invocation inside the expression"))

        call
