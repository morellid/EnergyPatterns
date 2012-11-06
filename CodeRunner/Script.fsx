// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Library1.fs"
open EnergyPatterns
open Microsoft.FSharp.Quotations

// Define your library scripting code here
//let kr = new KernelRunner()


type KernelRunner(expr: Expr) = 
    // To implement
    let isValid(e:Expr) =
      e.CustomAttributes
      true


type Kernel() =
  inherit System.Attribute()

// come cavolo si mettono i custom attributes in una Expr??
let expr : Expr<int> = <@ 1 + 1 @>


[<Kernel>]

[<ReflectedDefinition>]
let f a = a } 2  1

let expr = <@ f @>

a.
a.CustomAttributes

[<ReflectedDefinition>]
let expr : Expr<int> = <@ 1 + 1 @>
expr.CustomAttributes


// usando reflection standard (OK)
type Demo =
 [<Kernel>]
  static member AddOne x = x + 1

let ty = typeof<Demo>
let mi = ty.GetMethod("AddOne")
mi.CustomAttributes
typeof<Kernel>
let at = mi.GetCustomAttributes(typeof<Kernel>, false)
mi
