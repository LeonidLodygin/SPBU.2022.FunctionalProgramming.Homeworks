module SparseMatrix

open SparseVector
open System

type Matrix<'Value> =
    struct
        val Memory: 'Value option [,]
        val CoordinatesOfHead: uint * uint
        val Columns: uint
        val Lines: uint

        new(memory, head, columns, lines) =
            { Memory = memory
              CoordinatesOfHead = head
              Columns = columns
              Lines = lines }
    end

/// Quadrants: Fst:nw, Snd:ne, Thd:sw, Fth:se.
type QuadTree<'Value> =
    | Node of Fst: QuadTree<'Value> * Snd: QuadTree<'Value> * Thd: QuadTree<'Value> * Fth: QuadTree<'Value>
    | Leaf of 'Value
    | None

let Separator (matrix: Matrix<'Value>) =
    Matrix(matrix.Memory, matrix.CoordinatesOfHead, matrix.Columns / 2u, matrix.Lines / 2u),
    Matrix(
        matrix.Memory,
        (fst matrix.CoordinatesOfHead + matrix.Columns / 2u, snd matrix.CoordinatesOfHead),
        matrix.Columns / 2u,
        matrix.Lines / 2u
    ),
    Matrix(
        matrix.Memory,
        (fst matrix.CoordinatesOfHead, snd matrix.CoordinatesOfHead + matrix.Lines / 2u),
        matrix.Columns / 2u,
        matrix.Lines / 2u
    ),
    Matrix(
        matrix.Memory,
        (fst matrix.CoordinatesOfHead + matrix.Columns / 2u, snd matrix.CoordinatesOfHead + matrix.Lines / 2u),
        matrix.Columns / 2u,
        matrix.Lines / 2u
    )

let NoneDestroyer (tree: QuadTree<'Value>) =
    match tree with
    | Leaf value -> Leaf value
    | Node (None, None, None, None) -> None
    | _ -> tree

let ListMatrixSeparator list (size: uint) =
    let rec helper list nw ne sw se =
        match list with
        | [] -> nw, ne, sw, se
        | (x, y, value) :: tl ->
            if x < size / 2u then
                if y < size / 2u then
                    helper tl ((x, y, value) :: nw) ne sw se
                else
                    helper tl nw ((x, y - size / 2u, value) :: ne) sw se
            else if y < size / 2u then
                helper tl nw ne ((x - size / 2u, y, value) :: sw) se
            else
                helper tl nw ne sw ((x - size / 2u, y - size / 2u, value) :: se)

    helper list [] [] [] []

let MatrixFromList list size =
    let virtualLength = ClosestDegreeOf2 size 0u

    let rec helper list length =
        if length = 0u then
            QuadTree.None
        elif length = 1u
             && not (List.isEmpty list)
             && (First list.Head < size && Second list.Head < size) then
            QuadTree.Leaf(Third list.Head)
        elif length = 1u
             && ((List.isEmpty list)
                 || (First list.Head > size || Second list.Head > size)) then
            QuadTree.None
        else
            let nw, ne, sw, se = ListMatrixSeparator list length

            QuadTree.Node(
                helper nw (length / 2u),
                helper ne (length / 2u),
                helper sw (length / 2u),
                helper se (length / 2u)
            )
            |> NoneDestroyer

    helper list virtualLength

let Transformer (arr: 'Value option [,]) =
    let virtualLength =
        ClosestDegreeOf2(uint (Array2D.length1 arr)) (uint (Array2D.length2 arr))

    let virtualMatrix = Matrix(arr, (0u, 0u), virtualLength, virtualLength)

    let rec helper (virtualMatrix: Matrix<'Value>) =
        if fst virtualMatrix.CoordinatesOfHead
           >= uint (Array2D.length2 arr)
           || snd virtualMatrix.CoordinatesOfHead
              >= uint (Array2D.length1 arr) then
            None
        elif virtualMatrix.Columns = 1u
             && virtualMatrix.Lines = 1u then
            let uHead =
                try
                    Convert.ToInt32(fst virtualMatrix.CoordinatesOfHead),
                    Convert.ToInt32(snd virtualMatrix.CoordinatesOfHead)
                with
                | :? OverflowException ->
                    failwith $"%A{virtualMatrix.CoordinatesOfHead} is outside the range of the Int32 type."

            if virtualMatrix.Memory[snd uHead, fst uHead] = Option.None then
                None
            else
                Leaf virtualMatrix.Memory[snd uHead, fst uHead].Value
        else
            let fst, snd, thd, fth = Separator virtualMatrix

            Node(helper fst, helper snd, helper thd, helper fth)
            |> NoneDestroyer

    if Array2D.length1 arr = 0 || Array2D.length2 arr = 0 then
        None
    else
        helper virtualMatrix

type SparseMatrix<'Value when 'Value: equality> =
    val Memory: QuadTree<'Value>
    val Lines: uint
    val Columns: uint

    new(arr) =
        { Memory = Transformer arr
          Lines = uint (Array2D.length1 arr)
          Columns = uint (Array2D.length2 arr) }

    new(tree, lines, columns) =
        { Memory = tree
          Lines = lines
          Columns = columns }

    new(list, lines, columns) =
        { Memory = MatrixFromList list (max lines columns)
          Lines = lines
          Columns = columns }

    member this.Item
        with get (x, y) =
            if x >= this.Columns || y >= this.Lines then
                failwith $"Coordinates %A{x},{y} is out of range."
            else
                let virtualLength = ClosestDegreeOf2 this.Lines this.Columns

                let rec getElementByIndex tree columns lines x y =
                    match tree with
                    | None -> Option.None
                    | Leaf value -> Some value
                    | Node (fst, snd, thd, fth) ->
                        if x < columns / 2u then
                            if y < lines / 2u then
                                getElementByIndex fst (columns / 2u) (lines / 2u) x y
                            else
                                getElementByIndex thd (columns / 2u) (lines / 2u) x (y - lines / 2u)
                        else if y < lines / 2u then
                            getElementByIndex snd (columns / 2u) (lines / 2u) (x - columns / 2u) y
                        else
                            getElementByIndex fth (columns / 2u) (lines / 2u) (x - columns / 2u) (y - lines / 2u)

                getElementByIndex this.Memory virtualLength virtualLength x y
