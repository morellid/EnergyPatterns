namespace FSCL.Compiler.Processors

open FSCL.Compiler
open Microsoft.FSharp.Quotations
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Core.LanguagePrimitives

type ArrayAccessTransformation() =     
    let GetGenericMethodInfoFromExpr (q, ty:System.Type) = 
        let gminfo = 
            match q with 
            | Patterns.Call(_,mi,_) -> mi.GetGenericMethodDefinition()
            | _ -> failwith "unexpected failure decoding quotation at ilreflect startup"
        gminfo.MakeGenericMethod [| ty |]

    let GetArrayAccessMethodInfo ty =
        let get = GetGenericMethodInfoFromExpr(<@@ LanguagePrimitives.IntrinsicFunctions.GetArray<int> null 0 @@>, ty)
        let set = GetGenericMethodInfoFromExpr(<@@ LanguagePrimitives.IntrinsicFunctions.SetArray<int> null 0 0 @@>, ty)
        (get, set)

    let UpdateArrayAccessMode(var:string, mode:KernelParameterAccessMode, engine:TransformationStep) =
        if (engine.CompilerData("KERNEL_PARAMETER_TABLE")).IsNone then
            raise (CompilerException("KERNEL_PARAMETER_TABLE global data cannot be found, but it is required by ArrayAccessProcessor to execute"))
        
        let data = engine.CompilerData("KERNEL_PARAMETER_TABLE").Value :?> KernelParameterTable
        for pair in data do
            if pair.Key = var then
                let newMode = 
                    match mode, data.[pair.Key].Access with
                    | _, KernelParameterAccessMode.ReadWrite
                    | KernelParameterAccessMode.ReadWrite, _ ->
                        KernelParameterAccessMode.ReadWrite
                    | KernelParameterAccessMode.ReadOnly, KernelParameterAccessMode.WriteOnly
                    | KernelParameterAccessMode.WriteOnly, KernelParameterAccessMode.ReadOnly ->
                        KernelParameterAccessMode.ReadWrite
                    | _, _ ->
                        mode
                data.[pair.Key].Access <- newMode
            
    let GetSizeParameters(var, engine:TransformationStep) = 
        if (engine.CompilerData("KERNEL_PARAMETER_TABLE")).IsNone then
            raise (new CompilerException("KERNEL_PARAMETER_TABLE global data cannot be found, but it is required by ArrayAccessProcessor to execute"))
        
        let data = engine.CompilerData("KERNEL_PARAMETER_TABLE").Value :?> KernelParameterTable
        let mutable sizeParameters = []
                
        for pair in data do
            if pair.Key = var then
                sizeParameters <- pair.Value.SizeParameters
                
        if sizeParameters.IsEmpty then
            raise (CompilerException("Cannot determine the size variables of array " + var + ". This means it is not a kernel parameter or you are eploying aliasing"))
        sizeParameters
        
    let GetPlaceholderVar(var, engine:TransformationStep) = 
        if (engine.CompilerData("KERNEL_PARAMETER_TABLE")).IsNone then
            raise (new CompilerException("KERNEL_PARAMETER_TABLE global data cannot be found, but it is required by ArrayAccessProcessor to execute"))
        
        let data = engine.CompilerData("KERNEL_PARAMETER_TABLE").Value :?> KernelParameterTable
        let mutable placeholder = None
                
        for pair in data do
            if pair.Key = var then
                placeholder <- pair.Value.Placeholder
                
        if placeholder.IsNone then
            raise (CompilerException("Cannot determine the parameter referred by the kernel body " + var))
        placeholder.Value

    interface TransformationProcessor with
        member this.Handle(expr, engine:TransformationStep) =
            match expr with
            | Patterns.Call(o, methodInfo, args) ->
                if methodInfo.DeclaringType.Name = "IntrinsicFunctions" then
                    match args.[0] with
                    | Patterns.Var(v) ->
                        let arraySizeParameters = GetSizeParameters(v.Name, engine)
                        if methodInfo.Name = "GetArray" then
                            // Find the placeholder holding the variable of the flattened array
                            let placeholder = GetPlaceholderVar(v.Name, engine)
                            // Update the access mode of this array
                            UpdateArrayAccessMode(v.Name, KernelParameterAccessMode.ReadOnly, engine)
                            // Recursively process the arguments, except the array reference
                            let processedArgs = args |> List.tail |> List.map (fun (a:Expr) -> engine.Process(a))
                            NewExpr (Expr.Call(methodInfo, [Expr.Var(placeholder)] @ processedArgs))
                        elif methodInfo.Name = "GetArray2D" then
                            // Find the placeholder holding the variable of the flattened array
                            let placeholder = GetPlaceholderVar(v.Name, engine)
                            // Find the placeholders holding the array sizes
                            let sizePlaceHolders = List.map (fun (el:KernelParameterInfo) -> Expr.Var(el.Placeholder.Value)) (GetSizeParameters(v.Name, engine))
                            // Update the access mode of this array
                            UpdateArrayAccessMode(v.Name, KernelParameterAccessMode.ReadOnly, engine)
                            // Recursively process the arguments, except the array reference
                            let processedArgs = args |> List.tail |> List.map (fun (a:Expr) -> engine.Process(a))
                            let accessIndex = <@@ ((%%(processedArgs.[0]):int) * (%%(sizePlaceHolders.[1]):int)) + %%(processedArgs.[1]):int @@>
                            // Create a new call for the flattened array
                            let (get,set) = GetArrayAccessMethodInfo(v.Type.GetElementType())
                            NewExpr (Expr.Call(get, [Expr.Var(placeholder); accessIndex]))
                        elif methodInfo.Name = "GetArray3D" then 
                            // Find the placeholder holding the variable of the flattened array
                            let placeholder = GetPlaceholderVar(v.Name, engine)
                            // Find the placeholders holding the array sizes
                            let sizePlaceHolders = List.map (fun (el:KernelParameterInfo) -> Expr.Var(el.Placeholder.Value)) (GetSizeParameters(v.Name, engine))
                            // Update the access mode of this array
                            UpdateArrayAccessMode(v.Name, KernelParameterAccessMode.ReadOnly, engine)
                            // Recursively process the arguments, except the array reference                   
                            let processedArgs = args |> List.tail |> List.map (fun (a:Expr) -> engine.Process(a))
                            let accessIndex = <@@ ((%%(processedArgs.[0]):int) * (%%(sizePlaceHolders.[0]):int) * (%%(sizePlaceHolders.[1]):int)) + (%%(sizePlaceHolders.[0]):int) * (%%(processedArgs.[1]):int) + (%%(processedArgs.[2]):int) @@>
                            // Create a new call for the flattened array
                            let (get,set) = GetArrayAccessMethodInfo(v.Type.GetElementType())
                            NewExpr (Expr.Call(get, [Expr.Var(placeholder); accessIndex]))
                        elif methodInfo.Name = "SetArray" then
                            // Find the placeholder holding the variable of the flattened array
                            let placeholder = GetPlaceholderVar(v.Name, engine)
                            // Update the access mode of this array
                            UpdateArrayAccessMode(v.Name, KernelParameterAccessMode.WriteOnly, engine)
                            // Recursively process the arguments, except the array reference
                            let processedArgs = args |> List.tail |> List.map (fun (a:Expr) -> engine.Process(a))
                            // Create a new call for the flattened array
                            NewExpr (Expr.Call(methodInfo, [Expr.Var(placeholder)] @ processedArgs))
                        elif methodInfo.Name = "SetArray2D" then
                             // Find the placeholder holding the variable of the flattened array
                            let placeholder = GetPlaceholderVar(v.Name, engine)
                            // Find the placeholders holding the array sizes
                            let sizePlaceHolders = List.map (fun (el:KernelParameterInfo) -> Expr.Var(el.Placeholder.Value)) (GetSizeParameters(v.Name, engine))
                            // Update the access mode of this array
                            UpdateArrayAccessMode(v.Name, KernelParameterAccessMode.WriteOnly, engine)
                            // Recursively process the arguments, except the array reference
                            let processedArgs = args |> List.tail |> List.map (fun (a:Expr) -> engine.Process(a))
                            let accessIndex = <@@ ((%%(processedArgs.[0]):int) * (%%(sizePlaceHolders.[1]):int)) + %%(processedArgs.[1]):int @@>
                            // Create a new call for the flattened array
                            let (get,set) = GetArrayAccessMethodInfo(v.Type.GetElementType())
                            NewExpr (Expr.Call(set, [Expr.Var(placeholder); accessIndex; processedArgs.[2]]))
                         elif methodInfo.Name = "SetArray3D" then
                             // Find the placeholder holding the variable of the flattened array
                            let placeholder = GetPlaceholderVar(v.Name, engine)
                            // Find the placeholders holding the array sizes
                            let sizePlaceHolders = List.map (fun (el:KernelParameterInfo) -> Expr.Var(el.Placeholder.Value)) (GetSizeParameters(v.Name, engine))
                            // Update the access mode of this array
                            UpdateArrayAccessMode(v.Name, KernelParameterAccessMode.WriteOnly, engine)
                            // Recursively process the arguments, except the array reference
                            let processedArgs = args |> List.tail |> List.map (fun (a:Expr) -> engine.Process(a))
                            let accessIndex = <@@ ((%%(processedArgs.[0]):int) * (%%(sizePlaceHolders.[0]):int) * (%%(sizePlaceHolders.[1]):int)) + (%%(sizePlaceHolders.[0]):int) * (%%(processedArgs.[1]):int) + (%%(processedArgs.[2]):int) @@>
                            // Create a new call for the flattened array
                            let (get,set) = GetArrayAccessMethodInfo(v.Type.GetElementType())
                            NewExpr (Expr.Call(set, [Expr.Var(placeholder); accessIndex; processedArgs.[3]]))
                        else
                            Unhandled
                    | _ ->
                        Unhandled

                // Get length replaced with appropriate size parameter
                elif methodInfo.DeclaringType.Name = "Array" && methodInfo.Name = "GetLength" then
                    match o.Value with
                    | Patterns.Var(v) ->
                        let arrayName = v.Name
                        let arraySizeParameters = GetSizeParameters(arrayName, engine)
                        match args.[0] with
                        | Patterns.Value(v, ty) -> 
                            let sizePlaceholder = arraySizeParameters.[v :?> int].Placeholder
                            NewExpr (Expr.Var(sizePlaceholder.Value))
                        | _ -> 
                            raise (CompilerException("Cannot invoke GetLength using a non-contant parameter [array " + arrayName + "]"))
                    | _ ->
                        Unhandled
                else
                    Unhandled
            |_ ->
                Unhandled