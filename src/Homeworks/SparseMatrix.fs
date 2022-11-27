module SparseMatrix

open System

type Matrix<'value> =
    struct
        val Memory: 'value option [,]
        val Head: int * int
        val Columns: int
        val Lines: int

        new(memory, head, columns, lines) =
            { Memory = memory
              Head = head
              Columns = columns
              Lines = lines }
    end

type QuadTree<'value> =
    | Node of Fst: QuadTree<'value> * Snd: QuadTree<'value> * Thd: QuadTree<'value> * Fth: QuadTree<'value>
    | Leaf of 'value
    | None

let ClosestDegreeOf2 columns lines =
    int (
        2.0
        ** Math.Ceiling(Math.Log(float (max columns lines), 2.0))
    )

let Separator (matrix: Matrix<'value>) =
    Matrix(matrix.Memory, matrix.Head, matrix.Columns / 2, matrix.Lines / 2),
    Matrix(matrix.Memory, (fst matrix.Head + matrix.Columns / 2, snd matrix.Head), matrix.Columns / 2, matrix.Lines / 2),
    Matrix(matrix.Memory, (fst matrix.Head, snd matrix.Head + matrix.Lines / 2), matrix.Columns / 2, matrix.Lines / 2),
    Matrix(
        matrix.Memory,
        (fst matrix.Head + matrix.Columns / 2, snd matrix.Head + matrix.Lines / 2),
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
        if fst virtualMatrix.Head >= Array2D.length2 arr
           || snd virtualMatrix.Head >= Array2D.length1 arr then
            None
        elif virtualMatrix.Columns = 1
             && virtualMatrix.Lines = 1 then
            if virtualMatrix.Memory[snd virtualMatrix.Head, fst virtualMatrix.Head] = Option.None then
                None
            else
                Leaf
                    virtualMatrix.Memory[snd virtualMatrix.Head, fst virtualMatrix.Head]
                        .Value
        else
            let fst, snd, thd, fth = Separator virtualMatrix

            Node(helper (fst), helper (snd), helper (thd), helper (fth))
            |> NoneDestroyer

    if Array2D.length1 arr = 0 || Array2D.length2 arr = 0 then
        None
    else
        helper (virtualMatrix)

type SparseMatrix<'value when 'value: equality>(arr: 'value option [,]) =
    let memory = Transformer arr
    member this.Memory = memory
    member this.Lines = Array2D.length1 arr
    member this.Columns = Array2D.length2 arr

    member this.getItem x y =
        if x >= this.Columns || y >= this.Lines then
            failwith $"Coordinates %A{x},{y} is out of range."
        else
            let virtualLength = ClosestDegreeOf2(Array2D.length1 arr) (Array2D.length2 arr)

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
