module BreadthFirstSearch

open SparseVector
open SparseMatrix
open FSharp.Collections

let first (x, _, _) = x
let second (_, x, _) = x
let third (_, _, x) = x

let ListVecSeparator (list: List<int>) size =
    let rec helper list leftList rightList =
        match list with
        | [] -> leftList, rightList
        | hd :: tl ->
            if hd < size / 2 then
                helper tl (hd :: leftList) rightList
            else
                helper tl leftList ((hd - size / 2) :: rightList)

    helper list [] []

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

let VecFromList (list: List<int>) size =
    let virtualLength = ClosestDegreeOf2 size 0

    let rec helper list virtualLength =
        if virtualLength = 0 then
            BinaryTree.None
        elif virtualLength = 1
             && not (List.isEmpty list)
             && (list.Head < size) then
            BinaryTree.Leaf(true)
        elif virtualLength = 1
             && ((List.isEmpty list) || (list.Head >= size)) then
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
