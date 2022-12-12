module BreadthFirstSearch

open SparseVector
open SparseMatrix
open MultiMatrix
open FSharp.Collections

let first (x, _, _) = x
let second (_, x, _) = x
let third (_, _, x) = x

let ListMatrixSeparator (list: List<int * int * 'a option>) size =
    let rec helper list NW NE SW SE =
        match list with
        | [] -> NW, NE, SW, SE
        | (x, y, value) :: tl ->
            if x < size / 2 then
                if y < size / 2 then
                    helper tl ((x, y, value) :: NW) NE SW SE
                else
                    helper tl NW ((x, y - size / 2, value) :: NE) SW SE
            else if y < size / 2 then
                helper tl NW NE ((x - size / 2, y, value) :: SW) SE
            else
                helper tl NW NE SW ((x - size / 2, y - size / 2, value) :: SE)

    helper list [] [] [] []

let ListVecSeparator (list: List<int * 'a>) size =
    let rec helper list leftList rightList =
        match list with
        | [] -> leftList, rightList
        | (x, y) :: tl ->
            if x < size / 2 then
                helper tl ((x, y) :: leftList) rightList
            else
                helper tl leftList ((x - size / 2, y) :: rightList)

    helper list [] []

let VecFromList (list: List<int * 'a>) size =
    let virtualLength = ClosestDegreeOf2 size 0

    let rec helper list virtualLength =
        if virtualLength = 0 then
            BinaryTree.None
        elif virtualLength = 1
             && not (List.isEmpty list)
             && (fst list.Head < size) then
            BinaryTree.Leaf(snd list.Head)
        elif virtualLength = 1
             && ((List.isEmpty list) || (fst list.Head >= size)) then
            BinaryTree.None
        else
            let lists = ListVecSeparator list virtualLength

            BinaryTree.Node(helper (fst lists) (virtualLength / 2), helper (snd lists) (virtualLength / 2))
            |> SparseVector.NoneDestroyer

    SparseVector(helper list virtualLength, size)

let MatrixFromList (list: List<int * int * 'a option>) size =
    let virtualLength = ClosestDegreeOf2 size 0

    let rec helper list length =
        if length = 0 then
            QuadTree.None
        elif length = 1
             && not (List.isEmpty list)
             && (first list.Head < size && second list.Head < size) then
            QuadTree.Leaf(third list.Head)
        elif length = 1
             && ((List.isEmpty list)
                 || (first list.Head > size || second list.Head > size)) then
            QuadTree.None
        else
            let NW, NE, SW, SE = ListMatrixSeparator list length

            QuadTree.Node(
                helper NW (length / 2),
                helper NE (length / 2),
                helper SW (length / 2),
                helper SE (length / 2)
            )
            |> NoneDestroyer

    SparseMatrix(helper list virtualLength, size, size)

let FrontMult bool value =
    match bool, value with
    | Option.None, _ -> Option.None
    | Some true, Option.None -> Option.None
    | Some true, _ -> Some true
    | _ -> $"Something going wrong with fMult"

let FrontAdd bool bool2 =
    match bool, bool2 with
    | Option.None, Option.None -> Option.None
    | Some true, _ -> Some true
    | _, Some true -> Some true
    | _ -> $"Something going wrong with fAdd"

let Mask bool value =
    match bool, value with
    | Option.None, _ -> Option.None
    | Some true, Option.None -> Some true
    | Some true, _ -> Option.None
    | _ -> $"Something going wrong with Mask"

let SuperSum iter bool value =
    match bool, value with
    | Option.None, Option.None -> Option.None
    | Option.None, value -> value
    | Some true, Option.None -> Some iter
    | _ -> failwith $"Something wrong with SuperSum"

let Bfs (graph: List<int * int * 'a option>) (apexes: List<int * bool>) size =
    let matrix = MatrixFromList graph size
    let front = VecFromList apexes size

    let visited =
        FAddVector(SuperSum 0) front (SparseVector(Array.create front.Length Option.None))

    let rec helper (front: SparseVector<bool>) visited (iter: int) =
        if front.isEmpty then
            visited
        else
            let newFront =
                FAddVector Mask (MultiplyVecMat front matrix FrontAdd FrontMult) visited

            let visited = FAddVector(SuperSum iter) newFront visited
            helper newFront visited (iter + 1)

    helper front visited 1
