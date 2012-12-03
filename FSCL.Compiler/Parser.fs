namespace FSCL.Compiler

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations

type ParserProcessor<'T> =
    abstract member Handle : 'T * ParserStep<'T> -> (MethodInfo * Expr) option

and ParserStep<'T>() = 
    inherit CompilerStep<'T, Expr>()

    member val ParserProcessors = new List<ParserProcessor<'T>>() with get     
           
    member this.Process(expr:'T) =
        let mutable index = 0
        let mutable output = None
        while (output.IsNone) && (index < this.ParserProcessors.Count) do
            output <- this.ParserProcessors.[index].Handle(expr, this)
        if output.IsNone then
            raise (CompilerException("The engine is not able to parse a kernel inside the expression [" + expr.ToString() + "]"))
        output.Value

    override this.Run(expr) =
        let (signature, body) = this.Process(expr)
        this.AddCompilerData("KERNEL_SIGNATURE", signature)
        body
        

