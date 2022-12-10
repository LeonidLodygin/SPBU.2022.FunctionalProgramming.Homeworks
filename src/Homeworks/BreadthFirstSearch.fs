module BreadthFirstSearch

open SparseVector
open SparseMatrix
open FSharp.Collections

let third(_,_,x) = x
let rec ListSeparatorForMatrix (list:List<int*int*'a>) size : List<int*int*'a>*List<int*int*'a>*List<int*int*'a>*List<int*int*'a>=
    match list with
    | [] -> ([],[],[],[])
    | (f, s, t)::tl ->
        let fst, snd, thd, fth = ListSeparatorForMatrix tl size
        if f < size / 2 then
            if s < size/2 then
                ((f, s, t)::fst, snd, thd, fth)
            else
                (fst, (f - size/2, s, t)::snd, thd, fth)
        else
            if s < size/2 then
                (fst, snd, (f, s, t)::thd, fth)
            else
                (fst, snd, thd, (f - size/2, s - size/2, t)::fth)

let MatrixFromList (list:List<int*int*'a>) size =
    let virtualLength = ClosestDegreeOf2 size 0
    let rec helper list size =
        match list with
        | [] -> QuadTree.None
        | [hd] -> QuadTree.Leaf (third hd)
        | _ ->
            let fst, snd, thd, fth = ListSeparatorForMatrix list size
            let nw = helper fst (size/2)
            let ne = helper snd (size/2)
            let sw = helper thd (size/2)
            let se = helper fth (size/2)
            QuadTree.Node(nw, ne, sw, se)
    SparseMatrix(helper list virtualLength, size, size)


let rec ListSeparatorForVector (list:List<int>) size : List<int>*List<int> =
    match list with
    | [] -> ([],[])
    | hd::tl ->
        let left, right = ListSeparatorForVector tl size
        if hd < size / 2 then
            (hd::left, right)
        else
            (left, (hd-size/2)::right)

let VectorFromList (list:List<int>) size =
    let virtualLength = ClosestDegreeOf2 size 0
    let rec helper list size =
        match list with
        | [] -> BinaryTree.None
        | [hd] ->
            if hd > size then
                BinaryTree.None
            elif size = 1 then
                BinaryTree.Leaf true
            elif hd < size/2 then
                BinaryTree.Node(helper [hd] (size/2), BinaryTree.None)
            else
                BinaryTree.Node(BinaryTree.None, helper [hd-size/2] (size/2))
        | _ ->
            let divide = ListSeparatorForVector list size
            let left = helper (fst divide) (size/2)
            let right = helper (snd divide) (size/2)
            BinaryTree.Node(left, right)
    SparseVector(helper list virtualLength, size)

let vector = VectorFromList [1; 2; 3; 5] 6




//let Bfs graph points : SparseVector<int option> =

