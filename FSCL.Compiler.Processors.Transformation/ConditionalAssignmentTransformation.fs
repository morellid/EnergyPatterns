﻿namespace FSCL.Compiler.Processors

open FSCL.Compiler
open System.Collections.Generic
open System.Reflection
open System
open Microsoft.FSharp.Quotations

type ConditionalAssignmentTransformation() =   
    let rec MoveAssignmentIntoBody(var:Var, expr, engine:TransformationStep) =
        match expr with
        | Patterns.Sequential (e1, e2) ->
            Expr.Sequential(e1, MoveAssignmentIntoBody (var, e2, engine))
        | Patterns.IfThenElse(condinner, ifbinner, elsebinner) ->
            Expr.IfThenElse(condinner, MoveAssignmentIntoBody(var, ifbinner, engine), MoveAssignmentIntoBody(var, elsebinner, engine))     
        | Patterns.Let (e, v, body) ->
            Expr.Let(e, v, MoveAssignmentIntoBody(var, body, engine))
        | Patterns.Var (v) ->
            Expr.VarSet(var, Expr.Var(v))
        | Patterns.Value (v) ->
            Expr.VarSet(var, Expr.Value(v))
        | Patterns.Call (e, i, a) ->
            if e.IsSome then
                Expr.VarSet(var, Expr.Call(e.Value, i, a))
            else
                Expr.VarSet(var, Expr.Call(i, a))
        | _ ->
            raise (CompilerException("Cannot determine variable assignment in if-then-else construct. Try to transform v = if .. else ..; into v; if .. v <- .. else .. v <- .."))

    let rec MoveArraySetIntoBody(o:Expr option, mi:MethodInfo, a:Expr list, substituteIndex:int, expr, engine:TransformationStep) =
        match expr with
        | Patterns.Sequential (e1, e2) ->
            Expr.Sequential(e1, MoveArraySetIntoBody (o, mi, a, substituteIndex, e2, engine))
        | Patterns.IfThenElse(condinner, ifbinner, elsebinner) ->
            Expr.IfThenElse(condinner, MoveArraySetIntoBody(o, mi, a, substituteIndex, ifbinner, engine), MoveArraySetIntoBody(o, mi, a, substituteIndex, elsebinner, engine))     
        | Patterns.Let (e, v, body) ->
            Expr.Let(e, v, MoveArraySetIntoBody(o, mi, a, substituteIndex, body, engine))
        | Patterns.Var (v) ->
            Expr.Call(mi, List.mapi(fun i el -> if i = substituteIndex then Expr.Var(v) else el) a)
        | Patterns.Value (v, t) ->
            Expr.Call(mi, List.mapi(fun i el -> 
                if i = substituteIndex then 
                    Expr.Value(v, t)
                else el) a)
        | Patterns.Call (subo, subi, suba) ->
            if subo.IsSome then
                Expr.Call(mi, List.mapi(fun i el -> if i = substituteIndex then Expr.Call(subo.Value, subi, suba) else el) a)
            else
                Expr.Call(mi, List.mapi(fun i el -> if i = substituteIndex then Expr.Call(subi, suba) else el) a)
        | _ ->
            raise (CompilerException("Cannot determine variable assignment in if-then-else construct. Try to transform v = if .. else ..; into v; if .. v <- .. else .. v <- .."))

                                     
    interface TransformationProcessor with
        member this.Handle(expr, engine:TransformationStep) =
            match expr with
            | Patterns.Let(v, e, body) ->
                match e with
                | Patterns.IfThenElse(cond, ib, eb) ->                    
                    let fixedExpr = MoveAssignmentIntoBody(v, e, engine)
                    NewExpr (
                            Expr.Sequential(
                                Expr.Let(v, e, Expr.Value(0)),
                                engine.Process(fixedExpr)))
                | _ ->
                    Unhandled
            | Patterns.VarSet (v, e) ->
                match e with
                | Patterns.IfThenElse(cond, ib, eb) ->                    
                    let fixedExpr = MoveAssignmentIntoBody(v, e, engine)
                    NewExpr(engine.Process(fixedExpr))
                | _ ->
                    Unhandled                 
            | Patterns.Call (e, mi, a) ->
                if mi.DeclaringType.Name = "IntrinsicFunctions" then                    
                    if mi.Name.StartsWith "SetArray" then
                        let substituteIndex = a.Length - 1

                        match a.[substituteIndex] with
                        | Patterns.IfThenElse(cond, ib, eb) ->                    
                            let fixedExpr = MoveArraySetIntoBody(e, mi, a, substituteIndex, a.[substituteIndex], engine)
                            NewExpr(engine.Process(fixedExpr))
                        | _ ->
                           Unhandled
                    else
                        Unhandled
                else
                    Unhandled
            | _ ->
                Unhandled                   