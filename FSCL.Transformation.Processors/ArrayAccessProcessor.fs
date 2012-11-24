namespace FSCL.Transformation.Processors

open FSCL.Transformation
open Microsoft.FSharp.Quotations
open System.Collections.Generic
open System.Reflection

type ArrayAccessMode =
   | READ_ONLY
   | WRITE_ONLY
   | READ_WRITE

type ArrayAccessProcessor() =          
    let UpdateArrayAccessMode(var, mode:ArrayAccessMode, engine:KernelBodyTransformationStage) =
        let mutable data = Dictionary<string, ArrayAccessMode>()
        if (engine.TransformationData("ARRAY_ACCESS_MODES")).IsSome then
            data <- engine.TransformationData("ARRAY_ACCESS_MODES").Value :?> Dictionary<string, ArrayAccessMode>
        if data.ContainsKey(var) then
            let newMode = 
                match mode, data.[var] with
                | _, READ_WRITE
                | READ_WRITE, _ ->
                    READ_WRITE
                | READ_ONLY, WRITE_ONLY
                | WRITE_ONLY, READ_ONLY ->
                    READ_WRITE
                | _, _ ->
                    mode
            data.[var] <- newMode
            engine.AddTransformationData("ARRAY_ACCESS_MODES", data)
            
    let GetSizeParameters(var, engine:KernelBodyTransformationStage) =  
        let d = engine.TransformationData("SIGNATURE_ARRAY_SIZE_PARAMETERS") 
        if d.IsSome then
            let data = engine.TransformationData("SIGNATURE_ARRAY_SIZE_PARAMETERS").Value :?> Dictionary<ParameterInfo, string list>
            let mutable sizeParameters = []
            for k in data do
                if k.Key.Name = var then
                    sizeParameters <- k.Value
            if sizeParameters.IsEmpty then
                raise (KernelTransformationException("Cannot determine the size variables of array " + var + ". This means it is not a kernel parameter or you are eploying aliasing"))
            sizeParameters
        else
            raise (KernelTransformationException("Cannot find SIGNATURE_ARRAY_SIZE_PARAMETERS transformation data, which is required to execute ArrayAccessProcessor"))

    interface CallProcessor with
        member this.Handle(expr, o, methodInfo, args, engine:KernelBodyTransformationStage) =
            if methodInfo.DeclaringType.Name = "IntrinsicFunctions" then
                let arrayName = engine.Process(args.[0])
                let arraySizeParameters = GetSizeParameters(arrayName, engine)
                if methodInfo.Name = "GetArray" then
                    UpdateArrayAccessMode(arrayName, READ_ONLY, engine)
                    (true, Some(arrayName + "[" + engine.Process(args.[1]) + "]"))
                elif methodInfo.Name = "GetArray2D" then
                    UpdateArrayAccessMode(arrayName, READ_ONLY, engine)
                    let index = "(" + engine.Process(args.[1]) + ")" + " * " + arraySizeParameters.[0] + " + (" + engine.Process(args.[2]) + ")"
                    (true, Some(arrayName + "[" + index + "]"))
                elif methodInfo.Name = "GetArray3D" then
                    UpdateArrayAccessMode(arrayName, READ_ONLY, engine)
                    let index = "(" + engine.Process(args.[1]) + ")" + " * " + arraySizeParameters.[0] + " * " + arraySizeParameters.[1] + " + " + arraySizeParameters.[0] + " * (" + engine.Process(args.[2]) + ") + (" + engine.Process(args.[3]) + ")"
                    (true, Some(engine.Process(args.[0]) + "[" + index + "]"))
                elif methodInfo.Name = "SetArray" then
                    UpdateArrayAccessMode(arrayName, WRITE_ONLY, engine)
                    (true, Some(arrayName + "[" + engine.Process(args.[1]) + "] = " + engine.Process(args.[2])))
                elif methodInfo.Name = "SetArray2D" then
                    UpdateArrayAccessMode(arrayName, WRITE_ONLY, engine)
                    let index = "(" + engine.Process(args.[1]) + ")" + " * " + arraySizeParameters.[0] + " + (" + engine.Process(args.[2]) + ")"
                    (true, Some(arrayName + "[" + index + "]=" + engine.Process(args.[3])))
                elif methodInfo.Name = "SetArray3D" then
                    UpdateArrayAccessMode(arrayName, WRITE_ONLY, engine)
                    let index = "(" + engine.Process(args.[1]) + ")" + " * " + arraySizeParameters.[0] + " * " + arraySizeParameters.[1] + " + " + arraySizeParameters.[0] + " * (" + engine.Process(args.[2]) + ") + (" + engine.Process(args.[3]) + ")"
                    (true, Some(arrayName + "[" + index + "]=" + engine.Process(args.[4])))
                else
                    (false, None)

            // Get length replaced with appropriate size parameter
            elif methodInfo.DeclaringType.Name = "Array" && methodInfo.Name = "GetLength" then
                let arrayName = engine.Process(o.Value)
                let arraySizeParameters = GetSizeParameters(arrayName, engine)
                match args.[0] with
                | Patterns.Value(v, ty) -> 
                    (true, Some(arraySizeParameters.[v :?> int]))
                | _ -> 
                    raise (KernelTransformationException("Cannot invoke GetLength using a non-contant parameter [array " + arrayName + "]"))
            else
                (false, None)