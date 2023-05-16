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

let ParallelMultiplyVecMat
    (vector: SparseVector<'A>)
    (matrix: SparseMatrix<'B>)
    (fAdd: 'C option -> 'C option -> 'C option)
    (fMult: 'A option -> 'B option -> 'C option)
    parallelLevel
    =
    let rec helper (vectorTree: BinaryTree<'A>) (matrixTree: QuadTree<'B>) level : BinaryTree<'C> =
        match vectorTree, matrixTree with
        | BinaryTree.None, _ -> BinaryTree.None
        | _, QuadTree.None -> BinaryTree.None
        | BinaryTree.Leaf value, QuadTree.Leaf value1 -> fMult (Some value) (Some value1) |> NoneOrValue
        | Node (left, right), QuadTree.Node (first, second, third, fourth) ->
            if level = 0 then
                let vec1 = SparseVector(helper left first level, matrix.Columns)
                let vec2 = SparseVector(helper right third level, matrix.Columns)
                let vec3 = SparseVector(helper left second level, matrix.Columns)
                let vec4 = SparseVector(helper right fourth level, matrix.Columns)

                Node((ParallelFAddVector fAdd vec1 vec2 0).Memory, (ParallelFAddVector fAdd vec3 vec4 0).Memory)
                |> NoneDestroyer
            else
                let tasks =
                    [| async { return SparseVector(helper left first (level - 1), matrix.Columns) }
                       async { return SparseVector(helper right third (level - 1), matrix.Columns) }
                       async { return SparseVector(helper left second (level - 1), matrix.Columns) }
                       async { return SparseVector(helper right fourth (level - 1), matrix.Columns) } |]

                let results = tasks |> Async.Parallel |> Async.RunSynchronously

                Node(
                    (ParallelFAddVector fAdd results[0] results[1] 0)
                        .Memory,
                    (ParallelFAddVector fAdd results[2] results[3] 0)
                        .Memory
                )
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
            SparseVector((CutSomeTree(helper vector.Memory matrix.Memory parallelLevel) diffToCut), vector.Length)
        else
            let tree = GrowSomeTree vector.Memory diffToGrow
            SparseVector(CutSomeTree(helper tree matrix.Memory parallelLevel) diffToCut, matrix.Columns)
