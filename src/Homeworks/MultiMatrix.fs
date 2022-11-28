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
        | BinaryTree.Leaf value, QuadTree.Leaf value1 -> fMult (Some value) (Some value1) |> NoneOrValue
        | Node (left, right), QuadTree.Node (first, second, third, fourth) ->
            let vec1 = SparseVector(Helper left first, matrix.Columns)
            let vec2 = SparseVector(Helper right third, matrix.Columns)
            let vec3 = SparseVector(Helper left second, matrix.Columns)
            let vec4 = SparseVector(Helper right fourth, matrix.Columns)

            Node((FAddVector fAdd vec1 vec2).Memory, (FAddVector fAdd vec3 vec4).Memory)
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
