// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

open EnergyPatterns
open Microsoft.FSharp.Quotations

// Define your library scripting code here
//let kr = new KernelRunner()

let rec visit vars expr =
  let printval (v:Var) = 
    printf "ShapeVar:%s=%s " (v.Type.Name) (v.ToString())
  match expr with
  | Patterns.Let(v, e1, e2) -> printf "let %s:%s" (v.Type.Name) (v.ToString())
  | Patterns.Call(body, DerivedPatterns.MethodWithReflectedDefinition meth, args) ->
       match body with
       | Some b ->
          printf "call body:%s " (body.Value.ToString())
          printf " meth:%s " (meth.ToString())
       | _ ->
          printf "Call(" 
          visit Map.empty meth
          printf ") " 
  | ExprShape.ShapeVar v -> 
     printval v
  | ExprShape.ShapeLambda(v,e) -> 
     printf "ShapeLambda(" 
     printval v
     printf ","
     visit Map.empty e
     printf ")"
  | ExprShape.ShapeCombination(o,e) -> 
     printf "ShapeCombination( %s, " (o.GetType().Name) 
     List.iter (fun ex -> visit Map.empty ex) e
     printf ") "
  | _ -> printf "boh... "

[<ReflectedDefinition>]
let foo a = 
  let b = 1 in
    a + b

let e = <@ foo @>

visit Map.empty e
e


