namespace InstructionEnergyMetric

open Microsoft.FSharp.Quotations

module InstructionCountEstimator =
    let ReduceList (f: Expr -> float, l: Expr list) =
        l |> List.map f |> List.reduce(fun a b -> a + b)

    let rec Estimate (expr: Expr) =
        match expr with
        | Patterns.Let (var, v, b) ->
            (float) (Estimate(b))
        | Patterns.AddressOf (e) ->
            Estimate(e)
        | Patterns.AddressSet (e, v) ->
            Estimate(e) + Estimate(v)
        | Patterns.Application (a, b) ->
            Estimate(a) + Estimate(b)
        | Patterns.Coerce (e, t) ->
            Estimate(e)
        | Patterns.DefaultValue (t) ->
            0.0
        | Patterns.FieldGet (e, i) ->
            if e.IsSome then
                Estimate(e.Value)
            else
                0.0
        | Patterns.FieldSet (e, i, v) ->
            if e.IsSome then
                Estimate(e.Value) + Estimate(v)
            else
                Estimate(v)
        | Patterns.IfThenElse (c, ib, eb) ->
            Estimate(c) + (0.5 * Estimate(ib)) + (0.5 * Estimate(eb))
        | Patterns.Lambda (vr, e) ->
            Estimate(e)
        | Patterns.NewArray (t, l) ->
            ReduceList(Estimate, l)
        | Patterns.NewDelegate (t, vr, e) ->
            Estimate(e)
        | Patterns.NewObject (t, l) ->
           ReduceList(Estimate, l)
        | Patterns.NewRecord (t, l) ->
            ReduceList(Estimate, l)
        | Patterns.NewTuple (l) ->
            ReduceList(Estimate, l)
        | Patterns.NewUnionCase (i, l) ->
            ReduceList(Estimate, l)
        | Patterns.PropertyGet (e, i, l) ->
            if e.IsSome then
                Estimate(e.Value) + ReduceList(Estimate, l)
            else
                ReduceList(Estimate, l)
        | Patterns.PropertySet (e, i, l, v) ->
            if e.IsSome then
                Estimate(e.Value) + ReduceList(Estimate, l) + Estimate(v)
            else
                ReduceList(Estimate, l) + Estimate(v)
        | Patterns.Sequential (e1, e2) ->
            ReduceList(Estimate, [ e1; e2 ])
        | Patterns.TryFinally (e1, e2) ->
            ReduceList(Estimate, [ e1; e2 ])
        | Patterns.TupleGet (e, c) ->
            Estimate(e)
        | Patterns.TypeTest (e, t) ->
            Estimate(e)
        | Patterns.UnionCaseTest (e, i) ->
            Estimate(e)
        | Patterns.VarSet(v, e) ->
            Estimate(e)
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
                Estimate(e.Value) + ReduceList(Estimate, a) + 1.0
            else 
                ReduceList(Estimate, a) + 1.0
        | _ -> 0.0

