module GraphBuild

open System
open System.IO
open SparseMatrix


type Graph<'Value when 'Value: equality> =
    val Memory: SparseMatrix<'Value>
    val Vertices: uint
    val Edges: uint

    new(memory, vertices, edges) =
        { Memory = memory
          Vertices = vertices
          Edges = edges }


type ReadFile<'Value when 'Value: equality> =
    struct
        val NumericData: string
        val MatrixType: string
        val Vertices: uint
        val Entries: uint
        val SparseMatrix: SparseMatrix<'Value>

        new(numData, mType, vertices, entries, sparseMatrix) =
            { NumericData = numData
              MatrixType = mType
              Vertices = vertices
              Entries = entries
              SparseMatrix = sparseMatrix }
    end

let parser (dataType: Type) (data: string) =
    Convert.ChangeType(data, dataType, System.Globalization.CultureInfo.InvariantCulture.NumberFormat)

let MatrixReader (path: string) =
    if
        not (File.Exists(path))
        || FileInfo(path).Extension <> ".mtx"
    then
        failwith $"Couldn't find a file or this file has the wrong format"

    let splitter line =
        let pattern = "\s+"
        System.Text.RegularExpressions.Regex.Split(line, pattern)

    let allLines = File.ReadLines(path)
    let firstLine = splitter (Seq.head allLines)
    let numericData = firstLine[3]

    let dataType =
        match numericData with
        | "real" -> typeof<float>
        | "integer" -> typeof<int>
        | _ -> failwith $"Not expected data type"

    let matrixType = firstLine[4]
    let lines = Seq.skipWhile (fun (n: string) -> n[0] = '%') allLines
    let informationLine = splitter (Seq.head lines)
    let vertices = UInt32.Parse(informationLine[0])
    let entries = UInt32.Parse(informationLine[2])

    let rec helper list (lines: seq<string>) =
        match lines with
        | s when Seq.isEmpty s -> list
        | _ ->
            let coordinates = splitter (Seq.head lines)
            let weight = Some(parser dataType coordinates[2])

            let triple =
                ((UInt32.Parse coordinates[0]) - 1u, (UInt32.Parse coordinates[1]) - 1u, weight)

            if
                matrixType.Equals("symmetric")
                && coordinates[0] <> coordinates[1]
            then
                let secondTriple =
                    ((UInt32.Parse coordinates[1]) - 1u, (UInt32.Parse coordinates[0]) - 1u, weight)

                helper (triple :: secondTriple :: list) (Seq.skip 1 lines)
            else
                helper (triple :: list) (Seq.skip 1 lines)

    let list = helper [] (Seq.skip 1 lines)
    ReadFile(numericData, matrixType, vertices, entries, SparseMatrix(list, vertices, vertices))

let GraphBuilder (path: string) =
    let file = MatrixReader path
    Graph(file.SparseMatrix, file.Vertices, file.Entries)
