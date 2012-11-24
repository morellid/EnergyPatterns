namespace FSCL.Transformation

open System
open System.Reflection
open System.Collections.Generic
open Microsoft.FSharp.Quotations

type TransformationGlobalState = Dictionary<string, obj>

type TransformationStageBase() =
    let transformationData = TransformationGlobalState()

    member this.SetTransformationGlobalState(t:TransformationGlobalState) =
        transformationData.Clear()
        for pair in t do
            transformationData.Add(pair.Key, pair.Value)

    member this.AddTransformationData(s, d) =
        if transformationData.ContainsKey(s) then
            transformationData.[s] <- d
        else
            transformationData.Add(s, d)

    member this.RemoveTransformationData(s) =
        if transformationData.ContainsKey(s) then
            transformationData.Remove(s) |> ignore

    member this.TransformationData(s) =
        if transformationData.ContainsKey(s) then
            Some(transformationData.[s])
        else
            None

    member this.TransformationDataCopy
        with get() =
            new TransformationGlobalState(transformationData)

[<AbstractClass>]
type TransformationStage<'T,'U>() =
    inherit TransformationStageBase()

    abstract member Run: 'T * TransformationGlobalState -> 'U * TransformationGlobalState
