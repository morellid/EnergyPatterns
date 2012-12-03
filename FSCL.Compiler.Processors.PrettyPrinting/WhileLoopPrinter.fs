namespace FSCL.Compiler.Processors

open FSCL.Compiler
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations

type WhileLoopPrinter() =   
    interface BodyPrettyPrinterProcessor with
        member this.Handle(expr, engine:PrettyPrinterStep) =
            match expr with
            | Patterns.WhileLoop(cond, body) ->
                Some("while(" + engine.Process(cond) + ") {\n" + engine.Process(body) + "\n}\n")
            | _ ->
                None