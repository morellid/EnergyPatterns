namespace ComputationDensity

open System.Diagnostics
open System

module Tools =
    exception ExecuteForException of String

    let ExcuteFor time beforeStart afterStop f =
        let timer = Stopwatch()
        let mutable iterations = 1
        while (timer.ElapsedMilliseconds < 100L) do
            timer.Reset()
            timer.Start()
            for i in 0 .. iterations do
                f()
            timer.Stop()

            if (timer.ElapsedMilliseconds < 100L) then
                iterations <- iterations * 10

        let finalIterations = (int) (time * (double)iterations / ((double)timer.ElapsedMilliseconds))
        timer.Reset()
        let startResult = beforeStart() 
        if (startResult <> "OK") then
            raise (ExecuteForException(startResult))
        timer.Start()
        for i in 0 .. finalIterations - 1 do
            f()
        timer.Stop()
        (afterStop(), timer.ElapsedMilliseconds, finalIterations)
        

