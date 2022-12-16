module BreadthFirstSearch

open SparseVector
open SparseMatrix
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

let Bfs (graph: SparseMatrix<'value>) (apexes: List<uint>) =
    let apexes = List.map (fun x -> (x, ())) apexes
    let front = SparseVector(apexes, graph.Columns)

    let visited =
        FAddVector(SuperSum 0u) front (SparseVector(BinaryTree.None, graph.Columns))

    let rec Helper (front: SparseVector<'a>) visited iter =
        if front.isEmpty then
            visited
        else
            let newFront =
                FAddVector Mask (MultiplyVecMat front graph FrontAdd FrontMult) visited

            let visited = FAddVector(SuperSum iter) newFront visited
            Helper newFront visited (iter + 1u)

    Helper front visited 1u
