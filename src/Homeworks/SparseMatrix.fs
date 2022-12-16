module SparseMatrix

open SparseVector
open System

type Matrix<'value> =
    struct
        val Memory: 'value option [,]
        val CoordinatesOfHead: uint * uint
        val Columns: uint
        val Lines: uint

        new(memory, head, columns, lines) =
            { Memory = memory
              CoordinatesOfHead = head
              Columns = columns
              Lines = lines }
    end

/// Quadrants: Fst:NW, Snd:NE, Thd:SW, Fth:SE.
type QuadTree<'value> =
    | Node of Fst: QuadTree<'value> * Snd: QuadTree<'value> * Thd: QuadTree<'value> * Fth: QuadTree<'value>
    | Leaf of 'value
    | None

let Separator (matrix: Matrix<'value>) =
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

let NoneDestroyer (tree: QuadTree<'value>) =
    match tree with
    | Leaf value -> Leaf value
    | Node (None, None, None, None) -> None
    | _ -> tree

let ListMatrixSeparator list (size: uint) =
    let rec Helper list NW NE SW SE =
        match list with
        | [] -> NW, NE, SW, SE
        | (x, y, value) :: tl ->
            if x < size / 2u then
                if y < size / 2u then
                    Helper tl ((x, y, value) :: NW) NE SW SE
                else
                    Helper tl NW ((x, y - size / 2u, value) :: NE) SW SE
            else if y < size / 2u then
                Helper tl NW NE ((x - size / 2u, y, value) :: SW) SE
            else
                Helper tl NW NE SW ((x - size / 2u, y - size / 2u, value) :: SE)

    Helper list [] [] [] []

let MatrixFromList list size =
    let virtualLength = ClosestDegreeOf2 size 0u

    let rec Helper list length =
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
            let NW, NE, SW, SE = ListMatrixSeparator list length

            QuadTree.Node(
                Helper NW (length / 2u),
                Helper NE (length / 2u),
                Helper SW (length / 2u),
                Helper SE (length / 2u)
            )
            |> NoneDestroyer

    Helper list virtualLength

let Transformer (arr: 'value option [,]) =
    let virtualLength =
        ClosestDegreeOf2(uint (Array2D.length1 arr)) (uint (Array2D.length2 arr))

    let virtualMatrix = Matrix(arr, (0u, 0u), virtualLength, virtualLength)

    let rec helper (virtualMatrix: Matrix<'value>) =
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

type SparseMatrix<'value when 'value: equality> =
    val Memory: QuadTree<'value>
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

                let rec GetElementByIndex tree columns lines x y =
                    match tree with
                    | None -> Option.None
                    | Leaf value -> Some value
                    | Node (fst, snd, thd, fth) ->
                        if x < columns / 2u then
                            if y < lines / 2u then
                                GetElementByIndex fst (columns / 2u) (lines / 2u) x y
                            else
                                GetElementByIndex thd (columns / 2u) (lines / 2u) x (y - lines / 2u)
                        else if y < lines / 2u then
                            GetElementByIndex snd (columns / 2u) (lines / 2u) (x - columns / 2u) y
                        else
                            GetElementByIndex fth (columns / 2u) (lines / 2u) (x - columns / 2u) (y - lines / 2u)

                GetElementByIndex this.Memory virtualLength virtualLength x y
