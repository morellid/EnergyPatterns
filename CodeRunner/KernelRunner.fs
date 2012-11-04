namespace EnergyPatterns

open Microsoft.FSharp.Quotations

type KernelRunner(expr: Expr) = 
    // To implement
    let isValid: Expr -> bool
