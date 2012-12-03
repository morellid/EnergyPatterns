namespace FSCL.Compiler.Processors

open FSCL.Compiler
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations

type VarSetPrinter() =   
    interface BodyPrettyPrinterProcessor with
        member this.Handle(expr, engine:PrettyPrinterStep) =
            match expr with
            | Patterns.VarSet (v, e) ->
                Some(v.Name + " = " + engine.Process(e) + ";")
            | _ ->
                None