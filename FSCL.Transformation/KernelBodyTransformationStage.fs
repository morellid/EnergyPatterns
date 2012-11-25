namespace FSCL.Transformation

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations

type TypeProcessor =
    abstract member Handle : Type * KernelBodyTransformationStage -> bool * String option
    
and GenericProcessor =
    abstract member Handle : Expr * KernelBodyTransformationStage -> bool * String option

and SequentialProcessor =
    abstract member Handle : (Expr) * Expr * Expr * KernelBodyTransformationStage -> bool * String option

and IfThenElseProcessor =
    abstract member Handle : (Expr) * Expr * Expr * Expr * KernelBodyTransformationStage -> bool * String option

and IntegerRangeLoopProcessor =
    abstract member Handle : (Expr) * Var * Expr * Expr * Expr * KernelBodyTransformationStage -> bool * String option
    
and WhileLoopProcessor =
    abstract member Handle : (Expr) * Expr * Expr * KernelBodyTransformationStage -> bool * String option
    
and CallProcessor =
    abstract member Handle : (Expr) * Expr option * System.Reflection.MethodInfo * Expr list * KernelBodyTransformationStage -> bool * String option

and LetProcessor =
    abstract member Handle : (Expr) * Var * Expr * Expr * KernelBodyTransformationStage -> bool * String option
    
and VarSetProcessor =
    abstract member Handle : (Expr) * Var * Expr * KernelBodyTransformationStage -> bool * String option

and VarProcessor =
    abstract member Handle : Var * KernelBodyTransformationStage -> bool * String option

and ValueProcessor =
    abstract member Handle : Object * Type * KernelBodyTransformationStage -> bool * String option

and KernelBodyTransformationStage() = 
    inherit TransformationStage<(Expr * String), String>()

    member val TypeProcessors = new List<TypeProcessor>() with get
    member val CallProcessors = new List<CallProcessor>() with get
    member val LetProcessors = new List<LetProcessor>() with get
    member val IfThenElseProcessors = new List<IfThenElseProcessor>() with get
    member val IntegerRangeLoopProcessors = new List<IntegerRangeLoopProcessor>() with get
    member val WhileLoopProcessors = new List<WhileLoopProcessor>() with get
    member val SequentialProcessors = new List<SequentialProcessor>() with get
    member val VarSetProcessors = new List<VarSetProcessor>() with get
    member val VarProcessors = new List<VarProcessor>() with get
    member val ValueProcessors = new List<ValueProcessor>() with get
    member val GenericProcessors = new List<GenericProcessor>() with get
        
    member this.Process(v:Var) =
        let mutable index = 0
        let mutable output = None
        while (output.IsNone) && (index < this.VarProcessors.Count) do
            match this.VarProcessors.[index].Handle(v, this) with
            | (true, s) ->
                output <- s
            | (false, _) ->
                index <- index + 1
        if output.IsNone then
            raise (new KernelTransformationException("The engine found a variable reference that cannot be handled [" + v.ToString() + "]"))
        output.Value
        
    member this.Process(v:Object, t:Type) =
        let mutable index = 0
        let mutable output = None
        while (output.IsNone) && (index < this.ValueProcessors.Count) do
            match this.ValueProcessors.[index].Handle(v, t, this) with
            | (true, s) ->
                output <- s
            | (false, _) ->
                index <- index + 1
        if output.IsNone then
            raise (new KernelTransformationException("The engine found a value that cannot be handled [" + v.ToString() + ", of type " + t.Name + "]"))
        output.Value

    member this.Process(t: Type) =
        let mutable index = 0
        let mutable output = None
        while (output.IsNone) && (index < this.TypeProcessors.Count) do
            match this.TypeProcessors.[index].Handle(t, this) with
            | (true, s) ->
                output <- s
            | (false, _) ->
                index <- index + 1
        if output.IsNone then
            raise (new KernelTransformationException("The engine found a type that cannot be handled [" + t.ToString() + "]"))
        output.Value

    member this.Process(expression: Expr) =
        // At first, check generic processors (for complex constructs)
        let mutable index = 0
        let mutable output = None
        while (output.IsNone) && (index < this.GenericProcessors.Count) do
            match this.GenericProcessors.[index].Handle(expression, this) with
            | (true, s) ->
                output <- s
            | (false, _) ->
                index <- index + 1
        // If no suitable generic processor, use specific ones
        if output.IsSome then
            output.Value
        else
            match expression with
            | Patterns.Var(v) ->
                this.Process(v)
            | Patterns.Value(o, t) ->
                this.Process(o, t)

            | Patterns.Call(e, i, args) ->
                let mutable index = 0
                let mutable output = None
                while (output.IsNone) && (index < this.CallProcessors.Count) do
                    match this.CallProcessors.[index].Handle(expression, e, i, args, this) with
                    | (true, s) ->
                        output <- s
                    | (false, _) ->
                        index <- index + 1
                if output.IsNone then
                    raise (new KernelTransformationException("The engine found a call construct that cannot be handled [" + i.Name + "]"))
                output.Value
                
            | Patterns.VarSet (v, e) ->
                let mutable index = 0
                let mutable output = None
                while (output.IsNone) && (index < this.VarSetProcessors.Count) do
                    match this.VarSetProcessors.[index].Handle(expression, v, e, this) with
                    | (true, s) ->
                        output <- s
                    | (false, _) ->
                        index <- index + 1
                if output.IsNone then
                    raise (new KernelTransformationException("The engine found a varset that cannot be handled [" + v.Name + ", of type " + v.Type.Name + "]"))
                output.Value

            | Patterns.Let(variable, value, body) ->
                let mutable index = 0
                let mutable output = None
                while (output.IsNone) && (index < this.LetProcessors.Count) do
                    match this.LetProcessors.[index].Handle(expression, variable, value, body, this) with
                    | (true, s) ->
                        output <- s
                    | (false, _) ->
                        index <- index + 1
                if output.IsNone then
                    raise (new KernelTransformationException("The engine found a let binding construct that cannot be handled"))
                output.Value

            | Patterns.WhileLoop (condition, body) ->
                let mutable index = 0
                let mutable output = None
                while (output.IsNone) && (index < this.WhileLoopProcessors.Count) do
                    match this.WhileLoopProcessors.[index].Handle(expression, condition, body, this) with
                    | (true, s) ->
                        output <- s
                    | (false, _) ->
                        index <- index + 1
                if output.IsNone then
                    raise (new KernelTransformationException("The engine found a while loop construct that cannot be handled"))
                output.Value

            | Patterns.ForIntegerRangeLoop(variable, startexpr, endexpr, body) ->
                let mutable index = 0
                let mutable output = None
                while (output.IsNone) && (index < this.IntegerRangeLoopProcessors.Count) do
                    match this.IntegerRangeLoopProcessors.[index].Handle(expression, variable, startexpr, endexpr, body, this) with
                    | (true, s) ->
                        output <- s
                    | (false, _) ->
                        index <- index + 1
                if output.IsNone then
                    raise (new KernelTransformationException("The engine found a for loop construct that cannot be handled"))
                output.Value

            | Patterns.IfThenElse(condition, ifbranch, elsebranch) ->
                let mutable index = 0
                let mutable output = None
                while (output.IsNone) && (index < this.IfThenElseProcessors.Count) do
                    match this.IfThenElseProcessors.[index].Handle(expression, condition, ifbranch, elsebranch, this) with
                    | (true, s) ->
                        output <- s
                    | (false, _) ->
                        index <- index + 1
                if output.IsNone then
                    raise (new KernelTransformationException("The engine found an if then else construct that cannot be handled"))
                output.Value
       
            | Patterns.Sequential(expr1, expr2) ->
                let mutable index = 0
                let mutable output = None
                while (output.IsNone) && (index < this.SequentialProcessors.Count) do
                    match this.SequentialProcessors.[index].Handle(expression, expr1, expr2, this) with
                    | (true, s) ->
                        output <- s
                    | (false, _) ->
                        ()
                if output.IsNone then
                    raise (new KernelTransformationException("The engine found a sequential construct that cannot be handled"))
                output.Value

            | _ -> 
                raise (KernelTransformationException("Unrecognized construct in kernel " + expression.ToString()))
               
    override this.Run((body:Expr, signature:String)) =
        let body = this.Process(body)
        signature + " {\n" + body + "\n}\n"


