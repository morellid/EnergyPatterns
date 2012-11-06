
open Microsoft.FSharp.Quotations
type FuncType = 
| A of (int -> int -> int)
| B
| C

[<ReflectedDefinition>]
let add x y = x + y


let myFunc1 = A (fun x y -> x + y )
let myFunc2 = A add 

let thefunc expr = 
    match expr with
    | A(x) ->
        <@ x @>
    | _ ->
        failwith "fail"

printfn "%A" (thefunc myFunc1) // prints "Value (<fun:myFunc1@14>)"
printfn "%A" (thefunc myFunc2) // prints "Value (<fun:myFunc2@15>)"
printfn "%A" <@ fun x y -> x + y @> // generates usable expression tree