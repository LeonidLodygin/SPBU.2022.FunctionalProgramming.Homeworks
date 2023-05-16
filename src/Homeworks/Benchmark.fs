module Benchmark

open System
open GraphBuild
open SparseMatrix
open BenchmarkDotNet.Attributes
open BreadthFirstSearch

[<MemoryDiagnoser>]
type BfsBenchmark() =

    let mutable apexes = [ 1u ]
    let mutable matrix = SparseMatrix(array2D [| [||] |])
    let mutable graph = Graph(matrix, 0u, 0u)

    let GeneratorOfMatrix (length: int) (density: int) =
        let arr2D = Array2D.init (abs length) (abs length) (fun _ _ -> Option.None)
        let mutable totalElements = length * length * density / 100

        while totalElements <> 0 do
            let randomRow = Random().Next(length)
            let randomColumn = Random().Next(length)

            if arr2D[randomRow, randomColumn] = Option.None then
                arr2D[randomRow, randomColumn] <- Some(Random().Next(1, 100))
                totalElements <- totalElements - 1

        SparseMatrix arr2D


    [<Params(500, 1000, 2000, 4000, 8000)>]
    member val vertices: int = 0 with get, set

    [<Params(10, 20, 40, 60, 80, 90)>]
    member val density: int = 0 with get, set

    [<Params(0, 1, 2, 3, 4)>]
    member val parallelMult: int = 0 with get, set

    [<Params(0, 1, 2, 3, 4)>]
    member val parallelAdd: int = 0 with get, set

    [<GlobalSetup>]
    member this.Graph() =
        matrix <- GeneratorOfMatrix this.vertices this.density
        graph <- Graph(matrix, uint this.vertices, uint (this.density * this.vertices / 100))

    [<Benchmark(Baseline = true)>]
    member this.Bfs() =
        Bfs graph apexes this.parallelAdd this.parallelMult
