namespace FSCL

open Microsoft.FSharp.Quotations

type KernelInvocationException(msg: string) =
    inherit System.Exception(msg)
    
module Util =

    let rec GetKernelMethodInfoFromName expr =
        let call =            
            match expr with
            | Patterns.Lambda(v, e) -> 
                GetKernelMethodInfoFromName e
            | Patterns.Let (v, e1, e2) ->
                GetKernelMethodInfoFromName (e2)
            | Patterns.Call (e, i, a) ->
                match i with
                | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                    i
                | _ ->
                    raise (KernelInvocationException("A kernel invocation must provide a function marked with ReflectedDefinition attribute"))
            | _-> 
                raise (KernelInvocationException("Cannot find a kernel function invocation inside the expression"))

        call

    let GetKernelMethodInfoFromCall (expr:Expr) =
        match expr with
        | Patterns.Call (e, i, a) ->
            match i with
            | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                i
            | _ ->
                raise (KernelInvocationException("A kernel invocation must provide a function marked with ReflectedDefinition attribute"))
        | _-> 
            raise (KernelInvocationException("Cannot find a kernel function invocation inside the expression"))

