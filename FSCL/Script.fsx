
open Microsoft.FSharp.Quotations

[<ReflectedDefinition>]
let add = 
    <@@ 
        let mutable x = 0
        for i in 0..2..10 do
            x <- i
        x
    @@>

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
