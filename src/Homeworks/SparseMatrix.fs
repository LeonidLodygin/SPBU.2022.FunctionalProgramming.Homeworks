module SparseMatrix

open SparseVector

type Matrix<'value> =
    struct
        val Memory: 'value option [,]
        val CoordinatesOfHead: int * int
        val Columns: int
        val Lines: int

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
    Matrix(matrix.Memory, matrix.CoordinatesOfHead, matrix.Columns / 2, matrix.Lines / 2),
    Matrix(
        matrix.Memory,
        (fst matrix.CoordinatesOfHead + matrix.Columns / 2, snd matrix.CoordinatesOfHead),
        matrix.Columns / 2,
        matrix.Lines / 2
    ),
    Matrix(
        matrix.Memory,
        (fst matrix.CoordinatesOfHead, snd matrix.CoordinatesOfHead + matrix.Lines / 2),
        matrix.Columns / 2,
        matrix.Lines / 2
    ),
    Matrix(
        matrix.Memory,
        (fst matrix.CoordinatesOfHead + matrix.Columns / 2, snd matrix.CoordinatesOfHead + matrix.Lines / 2),
        matrix.Columns / 2,
        matrix.Lines / 2
    )

let NoneDestroyer (tree: QuadTree<'value>) =
    match tree with
    | Leaf value -> Leaf value
    | Node (None, None, None, None) -> None
    | _ -> tree

let Transformer (arr: 'value option [,]) =
    let virtualLength = ClosestDegreeOf2(Array2D.length1 arr) (Array2D.length2 arr)
    let virtualMatrix = Matrix(arr, (0, 0), virtualLength, virtualLength)

    let rec helper (virtualMatrix: Matrix<'value>) =
        if fst virtualMatrix.CoordinatesOfHead
           >= Array2D.length2 arr
           || snd virtualMatrix.CoordinatesOfHead
              >= Array2D.length1 arr then
            None
        elif virtualMatrix.Columns = 1
             && virtualMatrix.Lines = 1 then
            if virtualMatrix.Memory[snd virtualMatrix.CoordinatesOfHead, fst virtualMatrix.CoordinatesOfHead] = Option.None then
                None
            else
                Leaf
                    virtualMatrix.Memory[snd virtualMatrix.CoordinatesOfHead, fst virtualMatrix.CoordinatesOfHead]
                        .Value
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
    val Lines: int
    val Columns: int
    new(arr) =
        { Memory = Transformer arr
          Lines = Array2D.length1 arr
          Columns = Array2D.length2 arr }
    new(tree, lines, columns) =
        {
            Memory = tree
            Lines = lines
            Columns = columns
        }

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
                        if x < columns / 2 then
                            if y < lines / 2 then
                                GetElementByIndex fst (columns / 2) (lines / 2) x y
                            else
                                GetElementByIndex thd (columns / 2) (lines / 2) x (y - lines / 2)
                        else if y < lines / 2 then
                            GetElementByIndex snd (columns / 2) (lines / 2) (x - columns / 2) y
                        else
                            GetElementByIndex fth (columns / 2) (lines / 2) (x - columns / 2) (y - lines / 2)

                GetElementByIndex this.Memory virtualLength virtualLength x y
