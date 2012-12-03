namespace FSCL.Compiler.Processors

open FSCL.Compiler
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations

type UnionCasePrinter() =   
    interface BodyPrettyPrinterProcessor with
        member this.Handle(e, engine:PrettyPrinterStep) =
            match e with
            | Patterns.NewUnionCase(ui, args) ->
                if ui.DeclaringType.DeclaringType.Name = "fscl" then
                    Some(ui.Name)
                else
                    None
            | _ ->
                None