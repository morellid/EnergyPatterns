namespace EnergyPatterns

open Microsoft.FSharp.Quotations

type KernelRunner(expr: Expr) = 
    // To implement
    let isValid(e:Expr) =
      e.CustomAttributes
      true

