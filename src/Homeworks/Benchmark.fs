module Benchmark

open System
open GraphBuild
open SparseMatrix
open BenchmarkDotNet.Attributes
open BreadthFirstSearch

[<MemoryDiagnoser>]
type BfsBenchmark() =

    let mutable matrix = SparseMatrix(array2D [| [||] |])
    let mutable graph = Graph(matrix, 0u, 0u)

    let generatorOfMatrix (length: int) density =
        let arr2D =
            Array2D.init (abs length) (abs length) (fun _ _ ->
                if density <= 0.5 then
                    Option.None
                else
                    Some(Random().Next(1, 100)))

        let mutable totalElements =
            if density <= 0.5 then
                int (float length * float length * density)
            else
                length * length
                - int (float length * float length * density)

        while totalElements <> 0 do
            let randomRow = Random().Next(length)
            let randomColumn = Random().Next(length)

            if arr2D[randomRow, randomColumn] = Option.None
               && density <= 0.5 then
                arr2D[randomRow, randomColumn] <- Some(Random().Next(1, 100))
                totalElements <- totalElements - 1
            elif arr2D[randomRow, randomColumn] <> Option.None
                 && density > 0.5 then
                arr2D[randomRow, randomColumn] <- Option.None
                totalElements <- totalElements - 1

        SparseMatrix arr2D

    [<Params(1000, 4000)>]
    member val vertices: int = 0 with get, set

    [<Params(0.1, 0.9)>]
    member val density: float = 0 with get, set

    [<Params(1, 2, 3)>]
    member val parallelMult: int = 0 with get, set

    [<Params(1, 2, 3)>]
    member val parallelAdd: int = 0 with get, set

    [<GlobalSetup>]
    member this.Graph() =
        matrix <- generatorOfMatrix this.vertices this.density
        graph <- Graph(matrix, uint this.vertices, uint (this.density * float this.vertices))

    [<Benchmark(Baseline = true)>]
    member this.BfsBase() = Bfs graph [ 1u ] 0 0

    [<Benchmark>]
    member this.Bfs() =
        Bfs graph [ 1u ] this.parallelAdd this.parallelMult
