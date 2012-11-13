namespace FSCL

open Microsoft.FSharp.Quotations
open System 

type KernelBindingException(msg: string) =
    inherit Exception(msg)

type KernelBinding() =
    static member private ConvertType(t: Type) =
        if (t = typeof<int>) then
            "int"
        else
            raise (KernelBindingException("Invalid type used in kernel function " + t.ToString()))


    static member ConvertToCLKernel (expr: Expr) =
        let rec analyzeAndPrettyPrint expr =
            match expr with
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

        analyzeAndPrettyPrint(expr)
                
                
                
                

