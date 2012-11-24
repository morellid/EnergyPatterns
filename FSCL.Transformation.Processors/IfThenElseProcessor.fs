namespace FSCL.Transformation.Processors

open FSCL.Transformation
open System.Collections.Generic
open System.Reflection
open Microsoft.FSharp.Quotations

type DefaultIfThenElseProcessor() =   
    let rec LiftAndOrOperator(cond:Expr, ifb:Expr, elseb:Expr, engine:KernelBodyTransformationStage) =
        match ifb with
        | Patterns.Value(o, t) ->
            Some(engine.Process(cond) + " || " + engine.Process(elseb))
        | _ ->
            match elseb with  
            | Patterns.Value(o, t) ->
                Some(engine.Process(cond) + " && " + engine.Process(ifb))
            | _ ->
                None      

    interface IfThenElseProcessor with
        member this.Handle(expr, cond, ifb, elseb, engine:KernelBodyTransformationStage) =
            let checkBoolOp = LiftAndOrOperator(cond, ifb, elseb, engine)
            if checkBoolOp.IsSome then
                (true, checkBoolOp)
            else
                (true, Some("if(" + engine.Process(cond) + ") {\n" + engine.Process(ifb) + "}\nelse {\n" + engine.Process(elseb) + "\n}\n"))