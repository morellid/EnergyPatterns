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
        elif (t = typeof<bool>) then
            "bool"
        else
            raise (KernelBindingException("Invalid type used in kernel function " + t.ToString()))
         

    static member ConvertToCLKernel (expr: Expr) =
        let rec analyzeAndPrettyPrintCall (expr) =
            let binaryOp op (a:Expr list) =
                analyzeAndPrettyPrint(a.[0], false) + op + analyzeAndPrettyPrint(a.[1], false)
            let unaryOp op (a:Expr list) =
                op + analyzeAndPrettyPrint(a.[0], false)
            match expr with
            | DerivedPatterns.SpecificCall <@ (>) @> (e, t, a) -> binaryOp ">" a // relational operators
            | DerivedPatterns.SpecificCall <@ (<) @> (e, t, a) -> binaryOp "<" a
            | DerivedPatterns.SpecificCall <@ (>=) @> (e, t, a) -> binaryOp ">=" a
            | DerivedPatterns.SpecificCall <@ (<=) @> (e, t, a) -> binaryOp "<=" a
            | DerivedPatterns.SpecificCall <@ (=) @> (e, t, a) -> binaryOp "==" a
            | DerivedPatterns.SpecificCall <@ (<>) @> (e, t, a) -> binaryOp "!=" a
            | DerivedPatterns.SpecificCall <@ (+) @> (e, t, a) -> binaryOp "+" a  // aritmetic operators
            | DerivedPatterns.SpecificCall <@ (*) @> (e, t, a) -> binaryOp "*" a
            | DerivedPatterns.SpecificCall <@ (-) @> (e, t, a) -> binaryOp "-" a
            | DerivedPatterns.SpecificCall <@ (/) @> (e, t, a) -> binaryOp "/" a
            | DerivedPatterns.SpecificCall <@ (%) @> (e, t, a) -> binaryOp "%" a
            | DerivedPatterns.SpecificCall <@ (&&) @> (e, t, a) -> binaryOp "&&" a // logical operators
            | DerivedPatterns.SpecificCall <@ (||) @> (e, t, a) -> binaryOp "||" a
            | DerivedPatterns.SpecificCall <@ (&&&) @> (e, t, a) -> binaryOp "&" a  // bitwise operators
            | DerivedPatterns.SpecificCall <@ (|||) @> (e, t, a) -> binaryOp "|" a
            | DerivedPatterns.SpecificCall <@ (^^^) @> (e, t, a) -> binaryOp "^" a
            | DerivedPatterns.SpecificCall <@ (~~~) @> (e, t, a) -> binaryOp "~" a
            | DerivedPatterns.SpecificCall <@ (not) @> (e, t, a) -> unaryOp "!" a // unary
            | DerivedPatterns.SpecificCall <@ (>>>) @> (e, t, a) -> binaryOp ">>" a // shift
            | DerivedPatterns.SpecificCall <@ (<<<) @> (e, t, a) -> binaryOp "<<" a
            | _ ->
                raise (KernelBindingException("Invalid operator used in kernel function " + expr.ToString()))  

        and analyzeAndPrettyPrint(expr, cond) =
            match expr with
            | Patterns.Var (v) ->
                v.Name
            | Patterns.VarSet(variable, value) ->
                variable.Name + " = " + analyzeAndPrettyPrint(value, false) + ";\n"
            | Patterns.Call(e, i, args) ->
                analyzeAndPrettyPrintCall (expr)
            | Patterns.Value (v, ty) ->
                let t = KernelBinding.ConvertType(ty)
                v.ToString()
            | Patterns.Let(variable, value, body) ->
                KernelBinding.ConvertType(variable.Type) + " " + variable.Name + " = " + analyzeAndPrettyPrint(value, false) + ";\n" + analyzeAndPrettyPrint(body, false)
            | Patterns.WhileLoop (condition, body) ->
                "while(" + analyzeAndPrettyPrint(condition, true) + ") {\n" + analyzeAndPrettyPrint(body, false) + "\n}\n"
            | Patterns.ForIntegerRangeLoop(variable, startexpr, endexpr, body) ->
                "for(" + KernelBinding.ConvertType(variable.Type) + " " + variable.Name + " = " + analyzeAndPrettyPrint(startexpr, false) + "; " + variable.Name + " <= " + analyzeAndPrettyPrint(endexpr, true) + ";" + variable.Name + "++)\n{" + analyzeAndPrettyPrint(body, false) + "\n}\n"
            | Patterns.IfThenElse(condition, ifbranch, elsebranch) ->
                match cond with
                | false -> "if(" + analyzeAndPrettyPrint(condition, true) + ") {\n" + analyzeAndPrettyPrint(ifbranch, false) + "}\nelse {\n" + analyzeAndPrettyPrint(elsebranch, false) + "\n}\n"
                | true ->  analyzeAndPrettyPrint(condition, true) + "&&" + analyzeAndPrettyPrint(ifbranch, false)
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
                        "kernel " + i.Name + "(" + prettyArgs + ") {\n" + analyzeAndPrettyPrint(cleanBody, false) + "\n}\n"
                    | _ ->
                        ""
                | _ ->  
                    ""
        prettyPrint
                
                
                
                

