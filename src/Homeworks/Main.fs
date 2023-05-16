namespace Homeworks

open BenchmarkDotNet.Running
open Benchmark

module Main =

    [<EntryPoint>]
    let main (argv: string array) =
        BenchmarkRunner.Run<BfsBenchmark>() |> ignore
        0
