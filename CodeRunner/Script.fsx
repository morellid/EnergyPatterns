// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

open EnergyPatterns
open Microsoft.FSharp.Quotations

// Define your library scripting code here
//let kr = new KernelRunner()


[<ReflectedDefinition>]
let foo a = a * a
let e = <@ foo @>

match e with
  | ExprShape.ShapeVar v -> printf "ShapeVar"
  | ExprShape.ShapeLambda(v,e) -> printf "ShapeLambda"
  | ExprShape.ShapeCombination(o,e) -> printf "ShapeCombination"

