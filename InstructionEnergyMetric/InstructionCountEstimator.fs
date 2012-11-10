namespace InstructionEnergyMetric

open Microsoft.FSharp.Quotations
open System.Reflection
open MetricUtil

module InstructionCountEstimator =    
    let rec Estimate (expr: Expr, instrCount: Quotations.Var, args: ParameterInfo[]) =
        match expr with
        | Patterns.Let (var, v, b) ->
            Estimate(b, instrCount, args)
        | Patterns.AddressOf (e) ->
            Estimate(e, instrCount, args)
        | Patterns.AddressSet (e, v) ->
            EstimateList([ v; e ], instrCount, args) 
        | Patterns.Application (a, b) ->
            EstimateList ([ b; a ], instrCount, args)
        | Patterns.Coerce (e, t) ->
            Estimate(e, instrCount, args)
        | Patterns.DefaultValue (t) ->
            Expr.Value(0.0)
        | Patterns.FieldGet (e, i) ->
            if e.IsSome then
                Estimate(e.Value, instrCount, args)
            else
                Expr.Value(0.0)
        | Patterns.FieldSet (e, i, v) ->
            if e.IsSome then
                EstimateList([ v; e.Value ], instrCount, args)
            else
                Estimate(v, instrCount, args)
        | Patterns.IfThenElse (c, ib, eb) ->
            Estimate(c, args) + (0.5 * Estimate(ib, args)) + (0.5 * Estimate(eb, args))
        | Patterns.Lambda (vr, e) ->
            Estimate(e, instrCount, args)
        | Patterns.NewArray (t, l) ->
            EstimateList(l, instrCount, args)
        | Patterns.NewDelegate (t, vr, e) ->
            Estimate(e, instrCount, args)
        | Patterns.NewObject (t, l) ->
            EstimateList(l, instrCount, args)
        | Patterns.NewRecord (t, l) ->
            EstimateList(l, instrCount, args)
        | Patterns.NewTuple (l) ->
            EstimateList(l, instrCount, args)
        | Patterns.NewUnionCase (i, l) ->
            EstimateList(l, instrCount, args)
        | Patterns.PropertyGet (e, i, l) ->
            if e.IsSome then
                EstimateList(l @ [e.Value], instrCount, args)
            else
                EstimateList(l, instrCount, args)
        | Patterns.PropertySet (e, i, l, v) ->
            if e.IsSome then
                EstimateList(l @ [ v ; e.Value ], instrCount, args)
            else
                EstimateList(l @ [ v ], instrCount, args)
        | Patterns.Sequential (e1, e2) ->
            EstimateList([ e1; e2 ], instrCount, args)
        | Patterns.TryFinally (e1, e2) ->
            EstimateList([ e1; e2 ], instrCount, args)
        | Patterns.TupleGet (e, c) ->
            Estimate(e, instrCount, args)
        | Patterns.TypeTest (e, t) ->
            Estimate(e, instrCount, args)
        | Patterns.UnionCaseTest (e, i) ->
            Estimate(e, instrCount, args)
        | Patterns.VarSet(v, e) ->
            Estimate(e, instrCount, args)
        | Patterns.Call(e, i, a) ->
            EstimateCall(expr, instrCount, args)
        | _ -> 
            raise (MetricEvaluationError("Cannot build instruction evaluation lamba because of unrecognized patterns"))

    and EstimateList (l: Expr list, instrCount: Quotations.Var, args: ParameterInfo[]) =
        List.reduce (fun e1 e2 -> 
                        Expr.Sequential(
                            Estimate(e1, instrCount, args), 
                            Estimate(e2, instrCount, args))) l
                             
    and EstimateCall (expr:Expr, instrCount: Quotations.Var, args: ParameterInfo[]) =   
        match expr with 
        | DerivedPatterns.SpecificCall <@ (+) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (-) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (*) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (/) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (%) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (&&) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (||) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (&&&) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (|||) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (<<<) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (>>>) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (^^^) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (~-) @> (e, t, a) 
        | DerivedPatterns.SpecificCall <@ (~+) @> (e, t, a) ->
            if e.IsSome then
                Expr.Sequential(
                    EstimateList(a @ [ e.Value ], instrCount, args), 
                    Expr.VarSet(instrCount, Expr.Call((+), [ Expr.Var(instrCount); Expr.Value(1.0) ])))
            else 
                Expr.Sequential(
                    EstimateList(a, instrCount, args), 
                    Expr.VarSet(instrCount, Expr.Call((+), [ Expr.Var(instrCount); Expr.Value(1.0) ])))
        | _ ->
            raise (MetricEvaluationError("Cannot recognize some operations to count instructions"))
    
    and EstimateLoop (expr:Expr, args: ParameterInfo[]) =
        match expr with
        | Patterns.ForIntegerRangeLoop(v, startv, endv, body) ->
            0.0
        | _ ->
            0.0

    and EstimateValue (expr:Expr, args: ParameterInfo[]) =        
        match expr with 
        | DerivedPatterns.Bool (b) ->
            b :> obj
        | DerivedPatterns.Byte (b) ->
            b :> obj
        | DerivedPatterns.Char (b) ->
            b :> obj
        | DerivedPatterns.Double (b) ->
            b :> obj
        | DerivedPatterns.Int16 (b) ->
            b :> obj
        | DerivedPatterns.Int32 (b) ->
            b :> obj
        | DerivedPatterns.Int64 (b) ->
            b :> obj
        | DerivedPatterns.SByte (b) ->
            b :> obj
        | DerivedPatterns.Single (b) ->
            b :> obj
        | DerivedPatterns.UInt16 (b) ->
            b :> obj
        | DerivedPatterns.UInt16 (b) ->
            b :> obj
        | DerivedPatterns.UInt32 (b) ->
            b :> obj
        | DerivedPatterns.UInt64 (b) ->
            b :> obj
        | DerivedPatterns.Unit (b) ->
            b :> obj
        | _ ->
            null
    
    let EstimateInstructionCount (meth: MethodBase) =
        let args = meth.GetParameters()
        match meth with
        | DerivedPatterns.MethodWithReflectedDefinition (b) ->
            // Create a lambda to evaluate instruction count
            let lambda_body = Expr.Let(Quotations.Var("instr_count", typeof<double>, true), Expr.Value(0.0), Estimate(b, arg)) 
            Some(Array.foldBack (fun (arg:ParameterInfo) (expr:Expr) ->
                                    Expr.Lambda(
                                        Quotations.Var(arg.Name, arg.ParameterType, false), expr)) args lambda_body)
        | _ ->
            None


