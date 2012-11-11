namespace QuotationEvaluation

open System.Reflection
open System.Diagnostics
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Core.CompilerServices

type ExprContainer(ex: Expr) =
    let e = ex

    member this.Expr 
        with get() = e

[<TypeProvider>]
type QuotationEvaluatorTypeProvider(config: TypeProviderConfig) as this = 
    inherit TypeProviderForNamespaces()

    let namespaceName = "QuotationEvaluation"
    let thisAssembly = Assembly.GetExecutingAssembly()

    let t = ProvidedTypeDefinition(thisAssembly, namespaceName, "QuotationEvaluator", baseType = Some(typeof<ExprContainer>))
    let p = ProvidedParameter("expr", typeof<Expr>)
    let m = ProvidedMethod(methodName = "Eval", 
                           parameters = [],
                           returnType = typeof<obj>,
                           IsStaticMethod = true,
                           InvokeCode = fun a -> 
                                let arg = %%a.[0]
                                let cont = new ExprContainer(arg)
                                cont.Expr)
   
    do
        t.AddMember(m)    
    do
        this.AddNamespace(namespaceName, [ t ])
                           
[<assembly:TypeProviderAssembly>] 
do()
