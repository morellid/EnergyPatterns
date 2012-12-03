namespace FSCL.Compiler

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations

type PreprocessorProcessor =    
    abstract member Handle : Expr * PreprocessorStep -> unit

and PreprocessorStep() = 
    inherit CompilerStep<Expr, Expr>()

    member val PreprocessorProcessors = new List<PreprocessorProcessor>() with get

    member this.Process(expression: Expr) =
        let mutable index = 0       
        while index < this.PreprocessorProcessors.Count do
            this.PreprocessorProcessors.[index].Handle(expression, this)
            index <- index + 1
               
    override this.Run(body:Expr) =
        let signature = this.CompilerData("KERNEL_SIGNATURE").Value :?> MethodInfo
        this.Process(body)
        body


