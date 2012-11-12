namespace InstructionEnergyMetric

open Microsoft.FSharp.Quotations
open System.Reflection
open Microsoft.FSharp.Reflection
open MetricUtil
open Microsoft.FSharp.Linq.QuotationEvaluation

module InstructionCountEstimator =   
    // Checks if vars in an expression referes exclusively the parameters contained in a list
    let plusMethodInfo = InstructionEnergyMetricUtil.GetOperatorMethodInfo (<@ (+) @>, typeof<double>)
    let multMethodInfo = InstructionEnergyMetricUtil.GetOperatorMethodInfo (<@ (*) @>, typeof<double>)
    let subMethodInfo = InstructionEnergyMetricUtil.GetOperatorMethodInfo (<@ (-) @>, typeof<double>)
    let greatMethodInfo = InstructionEnergyMetricUtil.GetOperatorMethodInfo (<@ (>) @>, typeof<double>)

    let rec refersOnly (expr:Expr, pars:ParameterInfo[]) =
        match expr with
        | ExprShape.ShapeCombination (o, e) ->
            if e.IsEmpty then
                true
            else
                e |> List.map (fun exp -> refersOnly (exp, pars)) |> List.reduce (fun a b -> a && b)
        | ExprShape.ShapeLambda (v, e) ->
            if ((Array.filter (fun (p:ParameterInfo) -> p.Name = v.Name && v.Type = p.ParameterType) pars).Length = 0) then
                false
            else
                refersOnly(e, pars)
        | ExprShape.ShapeVar (v) ->
           (Array.filter (fun (p:ParameterInfo) -> p.Name = v.Name && v.Type = p.ParameterType) pars).Length <> 0
                         
    let rec Estimate (expr: Expr, args: ParameterInfo[]) =
        match expr with        
        | Patterns.Call(e, i, a) ->
            EstimateCall(expr, args)

        | Patterns.IfThenElse (c, ib, eb) ->
            Expr.Call(
                plusMethodInfo.Value,
                [ Estimate(c, args);
                  Expr.Call(
                    plusMethodInfo.Value,
                    [Expr.Call(
                        multMethodInfo.Value,
                        [ Expr.Value(0.5); Estimate(ib, args) ]);
                     Expr.Call(
                        multMethodInfo.Value,
                        [ Expr.Value(0.5); Estimate(eb, args) ])])])

        | Patterns.ForIntegerRangeLoop(v, startv, endv, body) ->
            // Check that startv is an expression of constants and fers to parameters
            if (refersOnly(startv, args) && refersOnly(endv, args)) then
                let subexpr = Estimate(body, args)
                let e = <@@
                            if ((%%startv : int) > (%%endv : int)) then
                                ((double)((%%startv : int) - (%%endv : int) + 1)) * (%%subexpr + 1.0)
                            else
                                ((double)((%%endv : int) - (%%startv : int) + 1)) * (%%subexpr + 1.0)
                        @@>
                e
            else
                raise (MetricEvaluationError("Cannot determine the loop count based on constants and function parameters"))

        | ExprShape.ShapeVar(var) ->
            Expr.Value (0.0)
        | ExprShape.ShapeLambda(var, lambda) ->
            Estimate (lambda, args)
        | ExprShape.ShapeCombination(o, e) ->        
            EstimateList (e, args)
        (*
        | Patterns.Let (var, v, b) ->
            EstimateList([ v; b ], args)
        | Patterns.AddressOf (e) ->
            Estimate(e, args)
        | Patterns.AddressSet (e, v) ->
            EstimateList([ v; e ], args) 
        | Patterns.Application (a, b) ->
            EstimateList ([ b; a ], args)
        | Patterns.Coerce (e, t) ->
            Estimate(e, args)
        | Patterns.DefaultValue (t) ->
            Expr.Value(0.0)
        | Patterns.FieldGet (e, i) ->
            if e.IsSome then
                Estimate(e.Value, args)
            else
                Expr.Value(0.0)
        | Patterns.FieldSet (e, i, v) ->
            if e.IsSome then
                EstimateList([ v; e.Value ], args)
            else
                Estimate(v, args)
        | Patterns.Lambda (vr, e) ->
            Estimate(e, args)
        | Patterns.NewArray (t, l) ->
            EstimateList(l, args)
        | Patterns.NewDelegate (t, vr, e) ->
            Estimate(e, args)
        | Patterns.NewObject (t, l) ->
            EstimateList(l, args)
        | Patterns.NewRecord (t, l) ->
            EstimateList(l, args)
        | Patterns.NewTuple (l) ->
            EstimateList(l, args)
        | Patterns.NewUnionCase (i, l) ->
            EstimateList(l, args)
        | Patterns.PropertyGet (e, i, l) ->
            if e.IsSome then
                EstimateList(l @ [e.Value], args)
            else
                EstimateList(l, args)
        | Patterns.PropertySet (e, i, l, v) ->
            if e.IsSome then
                EstimateList(l @ [ v ; e.Value ], args)
            else
                EstimateList(l @ [ v ], args)
        | Patterns.Sequential (e1, e2) ->
            EstimateList([ e1; e2 ], args)
        | Patterns.TryFinally (e1, e2) ->
            EstimateList([ e1; e2 ], args)
        | Patterns.TupleGet (e, c) ->
            Estimate(e, args)
        | Patterns.TypeTest (e, t) ->
            Estimate(e, args)
        | Patterns.UnionCaseTest (e, i) ->
            Estimate(e, args)
        | Patterns.VarSet(v, e) ->
            Estimate(e, args) *)
        | _ -> 
            raise (MetricEvaluationError("Cannot build instruction evaluation lamba because of unrecognized patterns"))

    and EstimateList (l: Expr list, args: ParameterInfo[]) =
        if l.IsEmpty then
            Expr.Value (0.0)
        else
            let result = ref (Estimate(l.[0], args))
            for i = 1 to l.Length - 1 do
                result := Expr.Call(plusMethodInfo.Value, [ !result; Estimate(l.[i], args) ])
            !result
                             
    and EstimateCall (expr:Expr, args: ParameterInfo[]) =   
        match expr with 
        (*| Patterns.Call(e, i, a) when i.DeclaringType.Name = "IntrinsicFunctions" && i.Name = "GetArray" ->
            if e.IsSome then
                EstimateList(a @ [ e.Value ], args)
            else 
                EstimateList(a, args)
        | Patterns.Call(e, i, a) when i.DeclaringType.Name = "IntrinsicFunctions" && i.Name = "SetArray" ->
            if e.IsSome then
                EstimateList(a @ [ e.Value ], args)
            else 
                EstimateList(a, args)    *)         
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
                Expr.Call(
                    plusMethodInfo.Value,
                    [ EstimateList(a @ [ e.Value ], args); Expr.Value(1.0) ])
            else 
                Expr.Call(
                    plusMethodInfo.Value,
                    [ EstimateList(a, args); Expr.Value(1.0) ])
        | Patterns.Call(e, i, a) ->
            if e.IsSome then
                EstimateList(a @ [ e.Value ], args)
            else
                EstimateList(a, args)
        | _ ->
            raise (MetricEvaluationError("Cannot recognize some operations to count instructions"))

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
    
    // Removes (0+0+0+0+0+) useless counts in the expression
    let rec CleanInstructionCount (expr: Expr) =
        if (Seq.isEmpty (expr.GetFreeVars())) then
            let value = expr.EvalUntyped() :?> double
            <@ value @> :> Expr
        else
            match expr with
            | Patterns.Lambda (v, e) ->
                Expr.Lambda(v, CleanInstructionCount(e))
            | Patterns.Let (v, e, b) ->
                Expr.Let(v, CleanInstructionCount(e), CleanInstructionCount(b))
            | Patterns.Call(e, i, [f; s]) ->  
                if (e.IsSome) then 
                    Expr.Call(CleanInstructionCount(e.Value), i, List.map (fun el -> CleanInstructionCount(el)) [f; s])
                else
                    Expr.Call(i, List.map (fun el -> CleanInstructionCount(el)) [f; s])
            | _ ->
                expr
    
    let EstimateInstructionCount (meth: MethodBase) =
        let args = meth.GetParameters()
        match meth with
        | DerivedPatterns.MethodWithReflectedDefinition (b) ->
            // Create a lambda to evaluate instruction count
            let lambdaBody = Estimate(b, args)
            let lc = CleanInstructionCount(lambdaBody)
            Some(lc)
        | _ ->
            None

        

