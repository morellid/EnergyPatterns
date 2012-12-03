namespace FSCL.Compiler.Processors

open FSCL.Compiler
open Microsoft.FSharp.Quotations
open System.Collections.Generic
open System.Reflection

type TypePrinter() =          
    interface TypePrettyPrinterProcessor with
        member this.Handle(t, engine:PrettyPrinterStep) =
            let arrayStar = if t.IsArray then "*" else ""
            let plainType = if t.IsArray then t.GetElementType() else t
            if (plainType = typeof<uint32>) then
                Some("unsigned int" + arrayStar)           
            elif (plainType = typeof<uint64>) then
                Some("unsigned long" + arrayStar) 
            elif (plainType = typeof<int64>) then
                Some("long" + arrayStar) 
            elif (plainType = typeof<int>) then
                Some("int" + arrayStar) 
            elif (plainType = typeof<double>) then
                Some("double" + arrayStar) 
            elif (plainType = typeof<float32>) then
                Some("float" + arrayStar) 
            elif (plainType = typeof<float>) then
                Some("float" + arrayStar) 
            else
                None