namespace MetricUtil

open System 

type MetricEvaluationError(msg: string) =
    inherit Exception(msg)

type MetricProfilingError(msg: string) =
    inherit Exception(msg)


