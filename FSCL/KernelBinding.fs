namespace FSCL

open Microsoft.FSharp.Quotations
open System 

type KernelBindingException(msg: string) =
    inherit Exception(msg)

type KernelBinding() =
    static member private ConvertType(t: Type) =
        if (t = typeof<int>) then
            "int"            
        elif (t = typeof<double>) then
            "double"
        elif (t = typeof<float32>) then
            "float"
        else
            raise (KernelBindingException("Invalid type used in kernel function " + t.ToString()))
         

    static member ConvertToCLKernel (expr: Expr) =
        let rec analyzeAndPrettyPrintCall (expr) =
            match expr with
            | DerivedPatterns.SpecificCall <@ (>) @> (e, t, a) ->
                analyzeAndPrettyPrint(a.[0]) + " > " + analyzeAndPrettyPrint(a.[1])
            | DerivedPatterns.SpecificCall <@ (+) @> (e, t, a) ->
               analyzeAndPrettyPrint(a.[0]) + " + " + analyzeAndPrettyPrint(a.[1])
            | _ ->
                raise (KernelBindingException("Invalid operator used in kernel function " + expr.ToString()))  

        and analyzeAndPrettyPrint expr =
            match expr with
            | Patterns.Var (v) ->
                v.Name
            | Patterns.VarSet(variable, value) ->
                variable.Name + " = " + analyzeAndPrettyPrint(value) + ";\n"
            | Patterns.Call(e, i, args) ->
                analyzeAndPrettyPrintCall (expr)
            | Patterns.Value (v, ty) ->
                let t = KernelBinding.ConvertType(ty)
                v.ToString()
            | Patterns.Let(variable, value, body) ->
                KernelBinding.ConvertType(variable.Type) + " " + variable.Name + " = " + analyzeAndPrettyPrint(value) + ";\n" + analyzeAndPrettyPrint(body)
            | Patterns.WhileLoop (condition, body) ->
                "while(" + analyzeAndPrettyPrint(condition) + ") {\n" + analyzeAndPrettyPrint(body) + "\n}\n"
            | Patterns.ForIntegerRangeLoop(variable, startexpr, endexpr, body) ->
                "for(" + KernelBinding.ConvertType(variable.Type) + " " + variable.Name + " = " + analyzeAndPrettyPrint(startexpr) + "; " + variable.Name + " <= " + analyzeAndPrettyPrint(endexpr) + ";" + variable.Name + "++)\n{" + analyzeAndPrettyPrint(body) + "\n}\n"
            | Patterns.IfThenElse(condition, ifbranch, elsebranch) ->
                "if(" + analyzeAndPrettyPrint(condition) + ") {\n" + analyzeAndPrettyPrint(ifbranch) + "}\nelse {\n" + analyzeAndPrettyPrint(elsebranch) + "\n}\n"
            | _ -> 
                raise (KernelBindingException("Unrecognized expression in kernel function " + expr.ToString()))
                
        let rec analyzeAndPrettyPrintArg expr =
            match expr with
            | Patterns.Var (v) ->
                KernelBinding.ConvertType(v.Type) + " " + v.Name
            | _ ->
                raise (KernelBindingException("Unrecognized parameter expression in kernel function " + expr.ToString()))

        let rec liftArgExtraction (expr, parameters: Reflection.ParameterInfo[]) =
            match expr with
            | Patterns.Lambda(v, e) ->
                liftArgExtraction (e, parameters)
            | Patterns.Let(v, value, body) ->
                let el = Array.tryFind (fun (p : Reflection.ParameterInfo) -> p.Name = v.Name) parameters
                if el.IsSome then
                    liftArgExtraction (body, parameters)
                else
                    expr
            | _ ->
                raise (KernelBindingException("Unrecognized parameter expression in kernel function " + expr.ToString()))

        let rec getKernelDefinition (expr) =
            match expr with
            | Patterns.Lambda(v, e) -> 
                getKernelDefinition e
            | Patterns.Let (v, e1, e2) ->
                getKernelDefinition (e2)
            | Patterns.Call (e, i, a) ->
                match i with
                | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                    expr
                | _ ->
                    raise (KernelBindingException("A kernel function must be marked with ReflectedDefinition attribute"))
            | _-> 
                raise (KernelBindingException("Cannot find a kernel function inside the expression"))

        let kernel = getKernelDefinition (expr)
        let prettyPrint =
            match kernel with
                | Patterns.Call (e, i, a) -> 
                    match i with
                    | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                        let prettyArgs = String.concat ", " (Seq.ofList (List.map (fun arg -> analyzeAndPrettyPrintArg(arg)) a))
                        let cleanBody = liftArgExtraction(b, i.GetParameters())
                        "kernel " + i.Name + "(" + prettyArgs + ") {\n" + analyzeAndPrettyPrint(cleanBody) + "\n}\n"
                    | _ ->
                        ""
                | _ ->  
                    ""
        prettyPrint
                
                
                
                

