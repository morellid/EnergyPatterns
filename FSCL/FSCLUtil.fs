namespace FSCL

open Microsoft.FSharp.Quotations
open System.Reflection
    
module Util =    
    let GetArrayDimensions (t:System.Type) =
        // If not array return 0
        if t.IsArray then
            // Any better way to do this?
            let dimensionsString = t.FullName.Split([| '['; ']' |]).[1]
            let dimensions = ref 1
            String.iter (fun c -> if (c = ',') then dimensions := !dimensions + 1) dimensionsString
            !dimensions
        else
            0

    let GetArrayCount (o) =
        if o.GetType().IsArray then
            Some(o.GetType().GetProperty("Length").GetValue(o) :?> int)
        else
            None

    let rec GetKernelFromName expr =
        let call =            
            match expr with
            | Patterns.Lambda(v, e) -> 
                GetKernelFromName e
            | Patterns.Let (v, e1, e2) ->
                GetKernelFromName (e2)
            | Patterns.Call (e, i, a) ->
                match i with
                | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                    (i, Array.map (fun (p:ParameterInfo) -> (p, GetArrayDimensions(p.ParameterType))) (i.GetParameters()))
                | _ ->
                    raise (KernelDefinitionException("A kernel definition must provide a function marked with ReflectedDefinition attribute"))
            | _-> 
                raise (KernelDefinitionException("Cannot find a kernel function definition inside the expression"))

        call

    let GetKernelFromCall (expr:Expr) =
        match expr with
        | Patterns.Call (e, i, a) ->
            match i with
            | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                (i, List.zip (List.ofArray (i.GetParameters())) a)
            | _ ->
                raise (KernelCallException("To be called, a kernel must provide a function marked with ReflectedDefinition attribute"))
        | _-> 
            raise (KernelCallException("Cannot find a kernel function invocation inside the expression"))

