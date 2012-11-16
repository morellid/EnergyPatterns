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
        elif (t = typeof<int[]>) then
            "int *"
        elif (t = typeof<int[,]>) then
            "int **"
        elif (t = typeof<int[,,]>) then
            "int ***"
        else
            raise (KernelBindingException("Invalid type used in kernel function " + t.ToString()))
         

    static member ConvertToCLKernel (expr: Expr) =
        
        let liftCallArgument (a:Expr) =
            " "        
        let rec liftAndOrOperator expr =
            match expr with
            | Patterns.IfThenElse(condinner, ifbinner, elsebinner) ->
                match ifbinner with
                | Patterns.Value(o, t) ->
                    if(t = typeof<bool>) then
                        if (o :?> bool) then
                            Some(analyzeAndPrettyPrint(condinner) + " || " + analyzeAndPrettyPrint(elsebinner))
                        else
                            None
                    else
                        None
                | _ ->
                    match elsebinner with  
                    | Patterns.Value(o, t) ->
                        if(t = typeof<bool>) then   
                            if (not (o :?> bool)) then
                                Some(analyzeAndPrettyPrint(condinner) + " && " + analyzeAndPrettyPrint(ifbinner))
                            else
                                None
                        else
                            None      
                    | _ ->
                    None      
            | _ ->
                None                     
                                
        and analyzeAndPrettyPrintCall (expr) =
            let binaryOp op (a:Expr list) =
                analyzeAndPrettyPrint(a.[0]) + op + analyzeAndPrettyPrint(a.[1])
            let unaryOp op (a:Expr list) =
                op + analyzeAndPrettyPrint(a.[0])
            match expr with
            | DerivedPatterns.SpecificCall <@ (>) @> (e, t, a) -> binaryOp " > " a // relational operators
            | DerivedPatterns.SpecificCall <@ (<) @> (e, t, a) -> binaryOp " < " a
            | DerivedPatterns.SpecificCall <@ (>=) @> (e, t, a) -> binaryOp " >= " a
            | DerivedPatterns.SpecificCall <@ (<=) @> (e, t, a) -> binaryOp " <= " a
            | DerivedPatterns.SpecificCall <@ (=) @> (e, t, a) -> binaryOp " == " a
            | DerivedPatterns.SpecificCall <@ (<>) @> (e, t, a) -> binaryOp " != " a
            | DerivedPatterns.SpecificCall <@ (+) @> (e, t, a) -> binaryOp " + " a  // aritmetic operators
            | DerivedPatterns.SpecificCall <@ (*) @> (e, t, a) -> binaryOp " * " a
            | DerivedPatterns.SpecificCall <@ (-) @> (e, t, a) -> binaryOp " - " a
            | DerivedPatterns.SpecificCall <@ (/) @> (e, t, a) -> binaryOp " / " a
            | DerivedPatterns.SpecificCall <@ (%) @> (e, t, a) -> binaryOp " % " a
            | DerivedPatterns.SpecificCall <@ (&&) @> (e, t, a) -> binaryOp " && " a // logical operators
            | DerivedPatterns.SpecificCall <@ (||) @> (e, t, a) -> binaryOp " || " a
            | DerivedPatterns.SpecificCall <@ (&&&) @> (e, t, a) -> binaryOp " & " a  // bitwise operators
            | DerivedPatterns.SpecificCall <@ (|||) @> (e, t, a) -> binaryOp " | " a
            | DerivedPatterns.SpecificCall <@ (^^^) @> (e, t, a) -> binaryOp " ^ " a
            | DerivedPatterns.SpecificCall <@ (~~~) @> (e, t, a) -> binaryOp " ~ " a
            | DerivedPatterns.SpecificCall <@ (not) @> (e, t, a) -> unaryOp " ! " a // unary
            | DerivedPatterns.SpecificCall <@ (>>>) @> (e, t, a) -> binaryOp " >> " a // shift
            | DerivedPatterns.SpecificCall <@ (<<<) @> (e, t, a) -> binaryOp " << " a
            | Patterns.Call(e,i,l) -> 
                "TODO GetLength" 
            | _ ->
                raise (KernelBindingException("Invalid operator used in kernel function " + expr.ToString()))  

        and analyzeAndPrettyPrint(expr) =
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
                let checkBoolOp = liftAndOrOperator(expr)
                if checkBoolOp.IsSome then
                    checkBoolOp.Value
                else
                    "if(" + analyzeAndPrettyPrint(condition) + ") {\n" + analyzeAndPrettyPrint(ifbranch) + "}\nelse {\n" + analyzeAndPrettyPrint(elsebranch) + "\n}\n"
            | Patterns.Sequential(expr1, expr2) ->
                analyzeAndPrettyPrint(expr1)  + ";\n" + analyzeAndPrettyPrint(expr2)  + ";\n"
            | _ -> 
                raise (KernelBindingException("Unrecognized expression in kernel function " + expr.ToString()))
                
        let rec analyzeAndPrettyPrintArg expr =
            match expr with
            | Patterns.Var (v) ->
                KernelBinding.ConvertType(v.Type) + " " + v.Name
            | Patterns.Call(e,i,a) ->
                liftCallArgument expr
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
                expr

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
                        let paramArgs = List.zip ((i.GetParameters()) |> List.ofArray) a
                        let prettyArgs = String.concat ", " (Seq.ofList (List.map (fun arg -> analyzeAndPrettyPrintArg(arg)) a))
                        let cleanBody = liftArgExtraction(b, i.GetParameters())
                        Some("kernel " + i.Name + "(" + prettyArgs + ") {\n" + analyzeAndPrettyPrint(cleanBody) + "\n}\n", paramArgs, i)
                    | _ ->
                        None
                | _ ->  
                    None
        prettyPrint
                
                
                
                

