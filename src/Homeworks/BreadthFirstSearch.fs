module BreadthFirstSearch

open SparseVector
open GraphBuild
open MultiMatrix
open FSharp.Collections

let FrontMult value1 value2 =
    match value1, value2 with
    | Option.None, _ -> Option.None
    | Some _, Option.None -> Option.None
    | Some value1, _ -> Some value1

let FrontAdd value1 value2 =
    match value1, value2 with
    | Option.None, Option.None -> Option.None
    | Some value1, _ -> Some value1
    | _, Some value2 -> Some value2

let Mask value1 value2 =
    match value1, value2 with
    | Option.None, _ -> Option.None
    | Some value1, Option.None -> Some value1
    | Some _, _ -> Option.None

let SuperSum iter value1 value2 =
    match value1, value2 with
    | Option.None, Option.None -> Option.None
    | Option.None, value2 -> value2
    | Some _, Option.None -> Some iter
    | _ -> failwith $"Something wrong with SuperSum"

let Bfs (graph: Graph<'Value>) (apexes: List<uint>) fAddLevel multLevel =
    let apexes = List.map (fun x -> (x, ())) apexes
    let front = SparseVector(apexes, graph.Vertices)

    let visited =
        ParallelFAddVector(SuperSum 0u) front (SparseVector(BinaryTree.None, graph.Vertices)) fAddLevel

    let rec helper (front: SparseVector<'A>) visited iter =
        if front.IsEmpty then
            visited
        else
            let newFront =
                ParallelFAddVector
                    Mask
                    (ParallelMultiplyVecMat front graph.Memory FrontAdd FrontMult multLevel)
                    visited
                    fAddLevel

            let visited = ParallelFAddVector(SuperSum iter) newFront visited fAddLevel
            helper newFront visited (iter + 1u)

    helper front visited 1u
