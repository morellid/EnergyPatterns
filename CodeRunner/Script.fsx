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
  | ExprShape.ShapeVar v -> 
     printval v
  | ExprShape.ShapeLambda(v,e) -> 
     printf "ShapeLambda(" 
     printval v
     printf ","
     visit Map.empty e
     printf ")"
  | ExprShape.ShapeCombination(o,e) -> 
     printf "ShapeCombination("
     List.iter (fun ex -> visit Map.empty ex) e
     printf ")"
  | _ -> printf "boh... "

  

[<ReflectedDefinition>]
let foo a = 
  let b = 1
  a + b

let e = <@ foo @>

visit Map.empty e



