
open Microsoft.FSharp.Quotations

[<ReflectedDefinition>]
let add x y = x + y

let rec analyse expr =
    match expr with
    | Patterns.Lambda(v, e) -> 
        printf("Lambda\n")
        analyse e
    | Patterns.Call (e, i, a) ->
        match i with
        | DerivedPatterns.MethodWithReflectedDefinition (b) ->
            printf("Found reflected body\n")
        | _ ->
            printf("No reflected body\n")
    | _-> printf("")
