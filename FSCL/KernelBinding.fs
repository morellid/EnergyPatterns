namespace FSCL

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core
open System 
open System.Collections.Generic
open FSCL.Transformation
open FSCL.Transformation.Processors

[<System.Flags>]
type ArrayAccessMode =
   | Read = 0x0001 
   | Write = 0x0002

type KernelBinding() =
    static let argsAccessMode = new Dictionary<string,ArrayAccessMode>()

    static member private BuildArrayLengthAdditionalArg (name:string) (n:obj) =
         String.Format("{0}_length_{1}", name, n.ToString())

    static member private BuildArrayLengthAdditionalArgs (name:string) (n:int) =
        String.concat ", int " (List.init n (KernelBinding.BuildArrayLengthAdditionalArg name))
        
    static member private ConvertType(t: Type) =
        let rec ConvertTypeInner(t: Type) =
            if (t = typeof<uint32>) then
                "unsigned int"            
            elif (t = typeof<uint64>) then
                "unsigned long"        
            elif (t = typeof<int64>) then
                "long"               
            elif (t = typeof<int>) then
                "int"            
            elif (t = typeof<double>) then
                "double"
            elif (t = typeof<float32>) then
                "float"
            elif (t = typeof<bool>) then
                "bool"
            elif (t.IsArray) then
                let dimensions = FSCL.Util.GetArrayDimensions(t)
                ConvertTypeInner(t.GetElementType()) + "*" //FIX: opencl doesn't allow pointer-to-pointer (String.replicate dimensions "*")
            else
                raise (KernelBindingException("Invalid type used in kernel function " + t.ToString()))
        
        ConvertTypeInner(t)

    static member ConvertToCLKernel (kernel: System.Reflection.MethodInfo) =        
        let liftCallArgument (a:Expr) =
            " "        
        let rec liftAndOrOperator expr =
            match expr with
            | Patterns.IfThenElse(condinner, ifbinner, elsebinner) ->
                match ifbinner with
                | Patterns.Value(o, t) ->
                    if(t = typeof<bool>) then
                        if (o :?> bool) then
                            Some(analyzeAndPrettyPrint(condinner) + " || " + analyzeAndPrettyPrint(elsebinner))
                        else
                            None
                    else
                        None
                | _ ->
                    match elsebinner with  
                    | Patterns.Value(o, t) ->
                        if(t = typeof<bool>) then   
                            if (not (o :?> bool)) then
                                Some(analyzeAndPrettyPrint(condinner) + " && " + analyzeAndPrettyPrint(ifbinner))
                            else
                                None
                        else
                            None      
                    | _ ->
                    None      
            | _ ->
                None                     
                                
        and analyzeAndPrettyPrintCall (expr) =
            let binaryOp op (a:Expr list) =
                "(" + analyzeAndPrettyPrint(a.[0]) + ")" + op + "(" + analyzeAndPrettyPrint(a.[1]) + ")"
            let unaryOp op (a:Expr list) =
                op + analyzeAndPrettyPrint(a.[0])
            match expr with
            | DerivedPatterns.SpecificCall <@ (>) @> (e, t, a) -> binaryOp " > " a // relational operators
            | DerivedPatterns.SpecificCall <@ (<) @> (e, t, a) -> binaryOp " < " a
            | DerivedPatterns.SpecificCall <@ (>=) @> (e, t, a) -> binaryOp " >= " a
            | DerivedPatterns.SpecificCall <@ (<=) @> (e, t, a) -> binaryOp " <= " a
            | DerivedPatterns.SpecificCall <@ (=) @> (e, t, a) -> binaryOp " == " a
            | DerivedPatterns.SpecificCall <@ (<>) @> (e, t, a) -> binaryOp " != " a
            | DerivedPatterns.SpecificCall <@ (+) @> (e, t, a) -> binaryOp " + " a  // aritmetic operators
            | DerivedPatterns.SpecificCall <@ (*) @> (e, t, a) -> binaryOp " * " a
            | DerivedPatterns.SpecificCall <@ (-) @> (e, t, a) -> binaryOp " - " a
            | DerivedPatterns.SpecificCall <@ (/) @> (e, t, a) -> binaryOp " / " a
            | DerivedPatterns.SpecificCall <@ (%) @> (e, t, a) -> binaryOp " % " a
            | DerivedPatterns.SpecificCall <@ (&&) @> (e, t, a) -> binaryOp " && " a // logical operators
            | DerivedPatterns.SpecificCall <@ (||) @> (e, t, a) -> binaryOp " || " a
            | DerivedPatterns.SpecificCall <@ (&&&) @> (e, t, a) -> binaryOp " & " a  // bitwise operators
            | DerivedPatterns.SpecificCall <@ (|||) @> (e, t, a) -> binaryOp " | " a
            | DerivedPatterns.SpecificCall <@ (^^^) @> (e, t, a) -> binaryOp " ^ " a
            | DerivedPatterns.SpecificCall <@ (~~~) @> (e, t, a) -> binaryOp " ~ " a
            | DerivedPatterns.SpecificCall <@ (not) @> (e, t, a) -> unaryOp " ! " a // unary
            | DerivedPatterns.SpecificCall <@ (>>>) @> (e, t, a) -> binaryOp " >> " a // shift
            | DerivedPatterns.SpecificCall <@ (<<<) @> (e, t, a) -> binaryOp " << " a
            | Patterns.Call(e,i,l) -> 
                let setArrayMode a m=
                    if (not(argsAccessMode.ContainsKey(a))) then
                        argsAccessMode.Add(a, m) |> ignore
                    else
                        let mode = argsAccessMode.[a]
                        argsAccessMode.[a] <- mode ||| m
                let raiseExc() = raise (KernelBindingException("Invalid operator used in kernel function " + expr.ToString()))
                if i.DeclaringType.Name = "fscl" then
                    // the function is defined in FSCL
                    let args = String.concat ", " (List.map (analyzeAndPrettyPrint) l)
                    i.Name + "(" + args + ")"
                else
                    if i.DeclaringType.Name = "IntrinsicFunctions" then
                        if i.Name = "GetArray" then
                            setArrayMode (l.[0].ToString()) (ArrayAccessMode.Read)
                            l.[0].ToString() + "[" + analyzeAndPrettyPrint(l.[1]) + "]"
                        elif i.Name = "GetArray2D" then
                            let arrayname = l.[0].ToString()
                            setArrayMode arrayname (ArrayAccessMode.Read)
                            let index = analyzeAndPrettyPrint(l.[1]) + "*" + KernelBinding.BuildArrayLengthAdditionalArg arrayname 0 + "+" + analyzeAndPrettyPrint(l.[2])
                            arrayname + "[" + index + "]"
                        elif i.Name = "GetArray3D" then
                            let arrayname = l.[0].ToString()
                            setArrayMode (l.[0].ToString()) (ArrayAccessMode.Read)
                            let index = analyzeAndPrettyPrint(l.[1]) + "*" + KernelBinding.BuildArrayLengthAdditionalArg arrayname 0  + "*" + (KernelBinding.BuildArrayLengthAdditionalArg arrayname 1) + "+" + analyzeAndPrettyPrint(l.[2]) + "*" + (KernelBinding.BuildArrayLengthAdditionalArg arrayname 1) + "+" + analyzeAndPrettyPrint(l.[3])
                            arrayname + "[" + index + "]"
                        elif i.Name = "SetArray" then
                            setArrayMode (l.[0].ToString()) (ArrayAccessMode.Write)
                            l.[0].ToString() + "[" + analyzeAndPrettyPrint(l.[1]) + "] = " + analyzeAndPrettyPrint(l.[2])
                        elif i.Name = "SetArray2D" then
                            let arrayname = l.[0].ToString()
                            setArrayMode arrayname (ArrayAccessMode.Write)
                            let index = analyzeAndPrettyPrint(l.[1]) + "*" + KernelBinding.BuildArrayLengthAdditionalArg arrayname 0 + "+" + analyzeAndPrettyPrint(l.[2])
                            arrayname + "[" + index + "]=" + analyzeAndPrettyPrint(l.[3])
                        elif i.Name = "SetArray3D" then
                            let arrayname = l.[0].ToString()
                            setArrayMode (l.[0].ToString()) (ArrayAccessMode.Write)
                            let index = analyzeAndPrettyPrint(l.[1]) + "*" + KernelBinding.BuildArrayLengthAdditionalArg arrayname 0  + "*" + (KernelBinding.BuildArrayLengthAdditionalArg arrayname 1) + "+" + analyzeAndPrettyPrint(l.[2]) + "*" + (KernelBinding.BuildArrayLengthAdditionalArg arrayname 1) + "+" + analyzeAndPrettyPrint(l.[3])
                            arrayname + "[" + index + "]=" + analyzeAndPrettyPrint(l.[4])
                        else
                            raiseExc()
                    elif i.DeclaringType.Name = "Array" && i.Name = "GetLength" then
                        match e with
                        | None -> raiseExc()
                        | Some(t) -> 
                            match t with
                            | Patterns.Var(ary) -> 
                                if l.Length < 1 then
                                    raiseExc()
                                else
                                    let first = l.[0]
                                    match first with
                                    | Patterns.Value(v, ty) -> KernelBinding.BuildArrayLengthAdditionalArg ary.Name v//ary.Name + "_length" + v.ToString()
                                    | _ -> raiseExc()
                                    
                            | _ -> raiseExc()                              
                    else
                        raiseExc()
            | _ ->
                raise (KernelBindingException("Invalid operator used in kernel function " + expr.ToString()))  

        and analyzeAndPrettyPrint(expr) =
            match expr with
            | Patterns.Var (v) ->
                v.Name
            | Patterns.VarSet(variable, value) ->
                variable.Name + " = " + analyzeAndPrettyPrint(value) + ";\n"
            | Patterns.Call(e, i, args) ->
                analyzeAndPrettyPrintCall (expr)
            | Patterns.Value (v, ty) ->
                let t = KernelBinding.ConvertType(ty)
                v.ToString()
            | Patterns.Let(variable, value, body) ->
                KernelBinding.ConvertType(variable.Type) + " " + variable.Name + " = " + analyzeAndPrettyPrint(value) + ";\n" + analyzeAndPrettyPrint(body)
            | Patterns.WhileLoop (condition, body) ->
                "while(" + analyzeAndPrettyPrint(condition) + ") {\n" + analyzeAndPrettyPrint(body) + "\n}\n"
            | Patterns.ForIntegerRangeLoop(variable, startexpr, endexpr, body) ->
                "for(" + KernelBinding.ConvertType(variable.Type) + " " + variable.Name + " = " + analyzeAndPrettyPrint(startexpr) + "; " + variable.Name + " <= " + analyzeAndPrettyPrint(endexpr) + ";" + variable.Name + "++)\n{" + analyzeAndPrettyPrint(body) + "\n}\n"
            | Patterns.IfThenElse(condition, ifbranch, elsebranch) ->
                let checkBoolOp = liftAndOrOperator(expr)
                if checkBoolOp.IsSome then
                    checkBoolOp.Value
                else
                    "if(" + analyzeAndPrettyPrint(condition) + ") {\n" + analyzeAndPrettyPrint(ifbranch) + "}\nelse {\n" + analyzeAndPrettyPrint(elsebranch) + "\n}\n"
            | Patterns.Sequential(expr1, expr2) ->
                analyzeAndPrettyPrint(expr1)  + ";\n" + analyzeAndPrettyPrint(expr2)  + ";\n"
            | _ -> 
                raise (KernelBindingException("Unrecognized expression in kernel function " + expr.ToString()))
                
        let rec analyzeAndPrettyPrintArg (p:System.Reflection.ParameterInfo) (fixedArg:string list, generatedArg:string list)=
            if p.ParameterType.IsArray then
                let dimensions = FSCL.Util.GetArrayDimensions(p.ParameterType)
                (fixedArg @ [("global " + KernelBinding.ConvertType(p.ParameterType) + " " + p.Name)], generatedArg @ [("int " + KernelBinding.BuildArrayLengthAdditionalArgs p.Name dimensions)])
            else
                (fixedArg @ [(KernelBinding.ConvertType(p.ParameterType) + " " + p.Name)], generatedArg)

        let rec liftArgExtraction (expr, parameters: Reflection.ParameterInfo[]) =
            match expr with
            | Patterns.Lambda(v, e) ->
                liftArgExtraction (e, parameters)
            | Patterns.Let(v, value, body) ->
                let el = Array.tryFind (fun (p : Reflection.ParameterInfo) -> p.Name = v.Name) parameters
                if el.IsSome then
                    liftArgExtraction (body, parameters)
                else
                    expr
            | _ ->
                expr

        let rec getKernelMethodBody (info) =
            match info with
            | DerivedPatterns.MethodWithReflectedDefinition(b) ->
                b
            | _ ->
                raise (KernelBindingException("A kernel function must be marked with ReflectedDefinition attribute"))
        argsAccessMode.Clear()
        let kernelBody = getKernelMethodBody (kernel)
        let kernelParams = kernel.GetParameters()
        let (fixedArg, generatedArg) =  Array.fold (fun (fixedState:string list, generatedState:string list) arg -> analyzeAndPrettyPrintArg arg (fixedState,generatedState) ) ([],[]) kernelParams //(fun arg -> analyzeAndPrettyPrintArg(arg)) kernelParams
        let prettyArgs = String.concat ", " (Seq.ofList (fixedArg @ generatedArg) )
        let cleanBody = liftArgExtraction(kernelBody, kernelParams)
        Some("kernel void " + kernel.Name + "(" + prettyArgs + ") {\n" + analyzeAndPrettyPrint(cleanBody) + ";\n}\n", 
            Seq.toList(seq {
                        for par in kernelParams do
                            if (argsAccessMode.ContainsKey(par.Name)) then
                                yield (par, Some(argsAccessMode.[par.Name]))
                            else
                                yield (par, None)
                        }))

        
    static member ConvertToCLKernel (kernel: Expr) =
        let (methodInfo, parameters) = FSCL.Util.GetKernelFromName(kernel)
        KernelBinding.ConvertToCLKernel(methodInfo)

    static member internal DefaultPipeline() =        
        let discovery = new KernelDiscoveryStage()
        let signature = new KernelSignatureTransformationStage()
        let body = new KernelBodyTransformationStage()

        discovery.DiscoveryProcessors.Add(new KernelByNameDiscoveryProcessor())

        signature.ParameterProcessors.Add(new DefaultParameterProcessor())
        signature.SignatureProcessors.Add(new DefaultSignatureProcessor())

        body.CallProcessors.Add(new ImplicitCallProcessor())
        body.CallProcessors.Add(new ArrayAccessProcessor())
        body.CallProcessors.Add(new ArithmeticOperationProcessor())

        body.IfThenElseProcessors.Add(new DefaultIfThenElseProcessor())
        body.IntegerRangeLoopProcessors.Add(new DefaultIntegerRangeLoopProcessor())
        body.WhileLoopProcessors.Add(new DefaultWhileLoopProcessor())
        body.LetProcessors.Add(new DefaultLetProcessor())
        body.SequentialProcessors.Add(new DefaultSequentialProcessor())
        body.VarProcessors.Add(new DefaultVarProcessor())
        body.VarSetProcessors.Add(new DefaultVarSetProcessor())
        body.ValueProcessors.Add(new DefaultValueProcessor())

        body.TypeProcessors.Add(new PlainTypeProcessor())
        body.TypeProcessors.Add(new ArrayTypeProcessor())

        // Run pipeline
        ((discovery --> signature) + signature) --> body

    static member Compile (kernel:Reflection.MethodInfo) =  
        let f = KernelBinding.DefaultPipeline()
        let result = f.Run(None, Some(kernel))
        (result, f.TransformationDataCopy)
        
    static member Compile (kernel:Expr) =  
        let f = KernelBinding.DefaultPipeline()
        let result = f.Run(Some(kernel), None)
        (result, f.TransformationDataCopy)
  
  
                
                
                
                

