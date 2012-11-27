namespace FSCL.Transformation.Processors

open FSCL.Transformation
open Microsoft.FSharp.Quotations

type ArithmeticOperationProcessor() =
    let HandleBinaryOp (op, a:Expr list, engine:KernelBodyTransformationStage) =
        "(" + engine.Process(a.[0]) + ")" + op + "(" + engine.Process(a.[1]) + ")"
    let HandleUnaryOp (op, a:Expr list, engine:KernelBodyTransformationStage) =
        op + engine.Process(a.[0])

    interface CallProcessor with
        member this.Handle(expr, o, methodInfo, args, engine) =
            match expr with
            | DerivedPatterns.SpecificCall <@ (>) @> (e, t, a) -> 
                (true, Some(HandleBinaryOp(" > ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (<) @> (e, t, a)  -> 
                (true, Some(HandleBinaryOp(" < ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (>=) @> (e, t, a)  -> 
                (true, Some(HandleBinaryOp(" >= ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (<=) @> (e, t, a)  -> 
                (true, Some(HandleBinaryOp(" <= ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (=) @> (e, t, a) -> 
                (true, Some(HandleBinaryOp(" == ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (<>) @> (e, t, a) -> 
                (true, Some(HandleBinaryOp(" != ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (+) @> (e, t, a) -> 
                (true, Some(HandleBinaryOp(" + ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (*) @> (e, t, a) -> 
                (true, Some(HandleBinaryOp(" * ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (-) @> (e, t, a) -> 
                (true, Some(HandleBinaryOp(" - ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (/) @> (e, t, a) -> 
                (true, Some(HandleBinaryOp(" / ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (%) @> (e, t, a) -> 
                (true, Some(HandleBinaryOp(" % ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (&&) @> (e, t, a) -> 
                (true, Some(HandleBinaryOp(" && ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (||) @> (e, t, a) ->
                (true, Some(HandleBinaryOp(" || ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (&&&) @> (e, t, a) -> 
                (true, Some(HandleBinaryOp(" & ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (|||) @> (e, t, a) -> 
                (true, Some(HandleBinaryOp(" | ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (^^^) @> (e, t, a) -> 
                (true, Some(HandleBinaryOp(" ^ ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (~~~) @> (e, t, a) -> 
                (true, Some(HandleUnaryOp(" ~ ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (not) @> (e, t, a) -> 
                (true, Some(HandleUnaryOp(" ! ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (>>>) @> (e, t, a) -> 
                (true, Some(HandleBinaryOp(" >> ", a, engine)))
            | DerivedPatterns.SpecificCall <@ (<<<) @> (e, t, a) -> 
                (true, Some(HandleBinaryOp(" << ", a, engine)))
            | _ ->
                (false, None)