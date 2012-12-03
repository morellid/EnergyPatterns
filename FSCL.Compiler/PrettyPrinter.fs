namespace FSCL.Compiler

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations

type SignaturePrettyPrinterProcessor =    
    abstract member Handle : MethodInfo * PrettyPrinterStep -> String option

and BodyPrettyPrinterProcessor =    
    abstract member Handle : Expr * PrettyPrinterStep -> String option
    
and TypePrettyPrinterProcessor =    
    abstract member Handle : Type * PrettyPrinterStep -> String option

and PrettyPrinterStep() = 
    inherit CompilerStep<Expr, String>()

    member val BodyPrettyPrinterProcessors = new List<BodyPrettyPrinterProcessor>() with get
    member val SignaturePrettyPrinterProcessors = new List<SignaturePrettyPrinterProcessor>() with get
    member val TypePrettyPrinterProcessors = new List<TypePrettyPrinterProcessor>() with get
        
    member this.Process(expression: Expr) =
        // At first, check generic processors (for complex constructs)
        let mutable index = 0
        let mutable output = None        
        while (output.IsNone) && (index < this.BodyPrettyPrinterProcessors.Count) do
            output <- this.BodyPrettyPrinterProcessors.[index].Handle(expression, this)
            index <- index + 1
        // If no suitable generic processor, use specific ones
        if (output.IsNone) then
            raise (CompilerException("Unrecognized construct in kernel body " + expression.ToString()))
        output.Value
        
    member this.Process(mi: MethodInfo) =
        // At first, check generic processors (for complex constructs)
        let mutable index = 0
        let mutable output = None        
        while (output.IsNone) && (index < this.SignaturePrettyPrinterProcessors.Count) do
            output <- this.SignaturePrettyPrinterProcessors.[index].Handle(mi, this)
            index <- index + 1
        // If no suitable generic processor, use specific ones
        if (output.IsNone) then
            raise (CompilerException("Unrecognized kernel signature " + mi.ToString()))
        output.Value
               
    member this.Process(t: Type) =
        // At first, check generic processors (for complex constructs)
        let mutable index = 0
        let mutable output = None        
        while (output.IsNone) && (index < this.TypePrettyPrinterProcessors.Count) do
            output <- this.TypePrettyPrinterProcessors.[index].Handle(t, this)
            index <- index + 1
        // If no suitable generic processor, use specific ones
        if (output.IsNone) then
            raise (CompilerException("Unrecognized kernel type " + t.ToString()))
        output.Value

    override this.Run(body:Expr) =
        let methodInfo = this.CompilerData("KERNEL_SIGNATURE").Value :?> MethodInfo
        this.Process(methodInfo) + "{\n" + this.Process(body) + "\n}\n"


