// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

open EnergyPatterns
open Microsoft.FSharp.Quotations

// Define your library scripting code here
//let kr = new KernelRunner()

// da studiare:
// http://fsharpsamples.codeplex.com/SourceControl/changeset/view/7af524066395#Samples%2fQuotationsVisualizer%2fQuotationsVisualizer%2fProgram.fs
// http://fortysix-and-two.blogspot.it/2009/06/traversing-and-transforming-f.html

let rec visit vars expr =
  let printval (v:Var) = 
    printf "ShapeVar:%s=%s " (v.Type.Name) (v.ToString())
  match expr with
  | Patterns.Let(v, e1, e2) -> 
     printf "Let (%s:%s, e1=" (v.Type.Name) (v.ToString())
     visit Map.empty e1
     printf ", e2="
     visit Map.empty e2
     printf ")"
  | Patterns.Call(body, DerivedPatterns.MethodWithReflectedDefinition meth, args) ->
       printf "Call(" 
       match body with
       | Some b ->
          printf "body="
          visit Map.empty b
       | _ -> ()
       printf "meth="
       visit Map.empty meth
       printf ", args=["
       Seq.iter (fun ar -> visit Map.empty meth) args
       printf "]) "
  | Patterns.Value(o, t) ->
     printf "Value(%s:%s)" (t.Name) (o.ToString())
  | Patterns.Application(e1,e2) ->  // non interessante
     printf "Application(e1="
     visit Map.empty e1
     printf ", e2="
     visit Map.empty e2
     printf ")"
  | Patterns.IfThenElse(e1,e2,e3) -> // esempio di controllo del flusso
     printf "If(condizione="
     visit Map.empty e1
     printf ", ramo if="
     visit Map.empty e2
     printf ", ramo else="
     visit Map.empty e3
     printf ")"
  | ExprShape.ShapeVar v -> 
     printval v
  | ExprShape.ShapeLambda(v,e) -> 
     printf "ShapeLambda(" 
     printval v
     printf ","
     visit Map.empty e
     printf ")"
  | ExprShape.ShapeCombination(o,e) ->  // qui va espanso meglio
     printf "ShapeCombination( o=%s, list=[" (o.ToString()) 
     match o with
     | Quotations.DerivedPatterns.Bool(e,b) -> printf "shape"
     List.iter (fun ex -> visit Map.empty ex) e
     printf "]) "
  | _ -> printf "boh... "

[<ReflectedDefinition>]
let foo a = 
  let b = 1 in
    a + b

[<ReflectedDefinition>]
let foo a = 
    if a < 0 then
      a - 1
    else
      a + 1

let e = <@ foo @>

visit Map.empty e
e


