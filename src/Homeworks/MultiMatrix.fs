module MultiMatrix

open Microsoft.FSharp.Core
open SparseMatrix
open SparseVector
open System

let rec GrowSomeTree tree diff =
    if diff > 0 then
        GrowSomeTree(Node(tree, BinaryTree.None)) (diff - 1)
    else
        tree

let rec CutSomeTree tree diff =
    match tree with
    | Node (first, _) when diff > 0 -> CutSomeTree first (diff - 1)
    | _ -> tree

let GetValue x =
    match x with
    | Some (value) -> value
    | _ -> failwith $"No value"

let FAddTree
    (func: 'a option -> 'b option -> 'c option)
    (vec1: SparseVector<'a>)
    (vec2: SparseVector<'b>)
    : SparseVector<'c> =
    let rec Helper (tree1: BinaryTree<'a>) (tree2: BinaryTree<'b>) : BinaryTree<'c> =
        match tree1, tree2 with
        | None, None -> BinaryTree.None
        | Leaf value1, Leaf value2 -> Leaf(func (Some value1) (Some value2) |> GetValue)
        | None, Leaf value -> Leaf(func Option.None (Some value) |> GetValue)
        | Leaf value, None -> Leaf(func (Some value) Option.None |> GetValue)
        | None, Node (left, right) -> Node(Helper None left, Helper None right)
        | Node (left, right), None -> Node(Helper left None, Helper right None)
        | Node (Leaf left, Leaf right), Node (Leaf left2, Leaf right2) ->
            Node(Leaf(func (Some left) (Some left2) |> GetValue), Leaf(func (Some right) (Some right2) |> GetValue))
        | Node (left, right), Node (left2, right2) -> Node(Helper left left2, Helper right right2)
        | _, _ -> failwith $"Something going wrong"

    if vec1.Length <> vec2.Length then
        failwith $"Different values of first vector length and second vector length"
    else
        SparseVector(Helper vec1.Memory vec2.Memory, vec1.Length)

let MultiplyVecMat
    (vector: SparseVector<'a>)
    (matrix: SparseMatrix<'b>)
    (fAdd: 'c option -> 'c option -> 'c option)
    (fMult: 'a option -> 'b option -> 'c option)
    =
    let rec Helper (vectorTree: BinaryTree<'a>) (matrixTree: QuadTree<'b>) : BinaryTree<'c> =
        match vectorTree, matrixTree with
        | BinaryTree.None, _ -> BinaryTree.None
        | _, QuadTree.None -> BinaryTree.None
        | BinaryTree.Leaf value, QuadTree.Leaf value1 -> BinaryTree.Leaf(fMult (Some value) (Some value1) |> GetValue)
        | Node (left, right), QuadTree.Node (first, second, third, fourth) ->
            let vec1 = SparseVector(Helper left first, matrix.Columns)
            let vec2 = SparseVector(Helper right third, matrix.Columns)
            let vec3 = SparseVector(Helper left second, matrix.Columns)
            let vec4 = SparseVector(Helper right fourth, matrix.Columns)

            Node((FAddTree fAdd vec1 vec2).Memory, (FAddTree fAdd vec3 vec4).Memory)
            |> NoneDestroyer
        | _, _ -> failwith $"Something going wrong"

    if vector.Length <> matrix.Lines then
        failwith $"Different values of vector length and matrix lines"
    else
        let vectorDegree = int (Math.Ceiling(Math.Log(float vector.Length, 2.0)))

        let matrixDegree =
            int (Math.Ceiling(Math.Log(float (max matrix.Lines matrix.Columns), 2.0)))

        let diffToGrow = matrixDegree - vectorDegree

        let diffToCut =
            vectorDegree
            - int (Math.Ceiling(Math.Log(float matrix.Columns, 2.0)))

        if vectorDegree = matrixDegree then
            SparseVector((CutSomeTree(Helper vector.Memory matrix.Memory) diffToCut), vector.Length)
        else
            let tree = GrowSomeTree vector.Memory diffToGrow
            SparseVector(CutSomeTree(Helper tree matrix.Memory) diffToCut, matrix.Columns)
