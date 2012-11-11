
open Microsoft.FSharp.Quotations

[<ReflectedDefinition>]
let add = 
    <@@ 
        fun x y ->
            let mutable b = 0
            b <- 1
            b <- 2
            b
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
