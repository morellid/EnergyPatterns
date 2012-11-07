// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FSCL

[<Kernel>]


[<EntryPoint>]
let main argv = 
    let t = new FSCL.OpenCLVector4D()
    t.s0
    t.xw <- [| 0.; 1. |]
    t.s1230 <- [| 1.;2.;3.;4. |]
    let g = t.even
    let g2 = t.odd
    printfn "%A" argv
    
    0 // return an integer exit code
