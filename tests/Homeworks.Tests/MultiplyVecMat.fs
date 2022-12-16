namespace Homeworks.Tests

open System
open Expecto
open Microsoft.FSharp.Core
open SparseMatrix
open SparseVector
open MultiMatrix


module SimpleTests =
    [<Tests>]
    let tests =
        testList
            "Some simple tests"
            [ testCase "Degree of 2 from 5"
              <| fun _ ->
                  let Result = ClosestDegreeOf2 5u 0u

                  Expect.equal
                  <| Result
                  <| 8u
                  <| "Closest degree of 2 from 5 is 2^3 = 8"
              testCase "Degree of 2 from zero"
              <| fun _ ->
                  let Result = ClosestDegreeOf2 0u 0u

                  Expect.equal
                  <| Result
                  <| 0u
                  <| "Closest degree of 2 from 0 is 0"
              testCase "Degree of 2 from 5 or 9"
              <| fun _ ->
                  let Result = ClosestDegreeOf2 5u 9u

                  Expect.equal
                  <| Result
                  <| 16u
                  <| "Closest degree of 2 from max of (5 or 9)  is 2^4 = 16"
              testCase "Separation of vector"
              <| fun _ ->
                  let Result =
                      Separator
                      <| Vector([| Some 1; Some 2; Some 3; Some 4 |], 0u, 4u)

                  Expect.equal
                  <| ((fst Result).Head, (fst Result).Length, (snd Result).Head, (snd Result).Length)
                  <| (0u, 2u, 2u, 2u)
                  <| "Separation of Vector [|Some 1; Some 2; Some 3; Some 4|] should be two vectors with lengths 2"
              testCase "Destroying nodes with nones"
              <| fun _ ->
                  let Result = NoneDestroyer <| Node(None, None)

                  Expect.equal
                  <| Result
                  <| None
                  <| "Node of None and None should be just None"
              testCase "Transforming tree from array"
              <| fun _ ->
                  let Result =
                      Transformer [| Some 1
                                     Some 2
                                     Some 3
                                     Option.None
                                     Some 5 |]

                  Expect.equal
                  <| Result
                  <| Node(Node(Node(Leaf 1, Leaf 2), Node(Leaf 3, None)), Node(Node(Leaf 5, None), None))
                  <| "Tree from [|Some 1; Some 2; Some 3; Option.None; Some 5|] should be Node(Node(Node(Leaf 1, Leaf 2), Node(Leaf 3, None)), Node(Node(Leaf 5, None), None))"
              testCase "Vector from array"
              <| fun _ ->
                  let Result =
                      SparseVector [| Some 1
                                      Some 2
                                      Some 3
                                      Some 4 |]

                  Expect.equal
                  <| (Result.Memory, Result.Length)
                  <| (Node(Node(Leaf 1, Leaf 2), Node(Leaf 3, Leaf 4)), 4u)
                  <| "SparseVector of [|Some 1; Some 2; Some 3; Some 4|] should be Node(Node(Leaf 1, Leaf 2), Node(Leaf 3, Leaf 4)) in Memory with Length 4"
              testCase "Separation of matrix"
              <| fun _ ->
                  let fst, snd, thd, fth =
                      SparseMatrix.Separator
                      <| Matrix(
                          array2D [ [ Some 1; Some 2 ]
                                    [ Some 3; Some 4 ] ],
                          (0u, 0u),
                          2u,
                          2u
                      )

                  Expect.equal
                  <| ((fst.Columns, fst.Lines),
                      (snd.Columns, snd.Lines),
                      (thd.Columns, thd.Lines),
                      (fth.Columns, fth.Lines))
                  <| ((1u, 1u), (1u, 1u), (1u, 1u), (1u, 1u))
                  <| "Separation of Matrix 2x2 should be 4 matrices 1x1"
              testCase "Destroying nodes with nones in matrices"
              <| fun _ ->
                  let Result =
                      SparseMatrix.NoneDestroyer
                      <| QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.None)

                  Expect.equal
                  <| Result
                  <| QuadTree.None
                  <| "Node of Nones should be just None"
              testCase "Tree from array2D"
              <| fun _ ->
                  let Result =
                      SparseMatrix.Transformer(
                          array2D [ [ Some 1; Some 2 ]
                                    [ Some 3; Some 4 ]
                                    [ Option.None; Some(5) ] ]
                      )

                  Expect.equal
                  <| Result
                  <| QuadTree.Node(
                      QuadTree.Node(QuadTree.Leaf 1, QuadTree.Leaf 2, QuadTree.Leaf 3, QuadTree.Leaf 4),
                      QuadTree.None,
                      QuadTree.Node(QuadTree.None, QuadTree.Leaf 5, QuadTree.None, QuadTree.None),
                      QuadTree.None
                  )
                  <| "Tree from [[Some 1; Some 2]; [Some 3; Some 4]; [Option.None; Some(5)]] should be Node(Node(Leaf 1, Leaf 2, Leaf 3, Leaf 4), None, Node(None, Leaf 5, None, None), None)"
              testCase "SparseMatrix from array2D"
              <| fun _ ->
                  let Result =
                      SparseMatrix(
                          array2D [ [ Some 1; Some 2 ]
                                    [ Some 3; Some 4 ]
                                    [ Option.None; Some(5) ] ]
                      )

                  Expect.equal
                  <| (Result.Memory, Result.Columns, Result.Lines)
                  <| (QuadTree.Node(
                          QuadTree.Node(QuadTree.Leaf 1, QuadTree.Leaf 2, QuadTree.Leaf 3, QuadTree.Leaf 4),
                          QuadTree.None,
                          QuadTree.Node(QuadTree.None, QuadTree.Leaf 5, QuadTree.None, QuadTree.None),
                          QuadTree.None
                      ),
                      2u,
                      3u)
                  <| "SparseMatrix from [[Some 1; Some 2]; [Some 3; Some 4]; [Option.None; Some(5)]] should be SparseMatrix with 2 columns and 3 lines and with Node(Node(Leaf 1, Leaf 2, Leaf 3, Leaf 4), None, Node(None, Leaf 5, None, None), None) in Memory"
              testCase "Let's cut some trees!"
              <| fun _ ->
                  let Result =
                      CutSomeTree
                      <| Node(Node(Leaf 1, None), Node(Leaf 2, Leaf 4))
                      <| 1

                  Expect.equal
                  <| Result
                  <| Node(Leaf 1, None)
                  <| "CutSomeTree of Node(Node(Leaf 1, None), Node(Leaf 2, Leaf 4)) with difference 1 should be just Node(Leaf 1, None)"
              testCase "Let's grow some trees!"
              <| fun _ ->
                  let Result = GrowSomeTree <| Node(Leaf 1, None) <| 1

                  Expect.equal
                  <| Result
                  <| Node(Node(Leaf 1, None), None)
                  <| "GrowSomeTree of Node(Leaf 1, None) with difference 1 should be Node(Node(Leaf 1, None), None)" ]

module MultiplyTests =
    let fAdd a b =
        match a, b with
        | Some x, Some y ->
            if x + y <> 0 then
                Some(x + y)
            else
                Option.None
        | Option.None, Some x -> Some x
        | Some x, Option.None -> Some x
        | Option.None, Option.None -> Option.None

    let fMult a b =
        match a, b with
        | Some x, Some y ->
            if x * y <> 0 then
                Some(x * y)
            else
                Option.None
        | Option.None, _
        | _, Option.None -> Option.None

    [<Tests>]
    let tests =
        testList
            "Some multiply tests"
            [ testCase "Multiply vec on matrix"
              <| fun _ ->
                  let vec = SparseVector [| Some 1; Some 2 |]

                  let matrix =
                      SparseMatrix
                      <| array2D [ [ Some 2; Some 3; Some 4 ]
                                   [ Some 1; Option.None; Some 5 ] ]

                  let Result = MultiplyVecMat vec matrix fAdd fMult

                  Expect.equal
                  <| (Result.Memory, Result.Length)
                  <| (Node(Node(Leaf 4, Leaf 3), Node(Leaf 14, None)), 3u)
                  <| "Multiply SparseVector from [|Some 1; Some 2|] on SparseMatrix from array2D [[Some 2; Some 3; Some 4]; [Some 1; Option.None; Some 5]] should be SparseVector with Node(Node(Leaf 4, Leaf 3), Node(Leaf 14, None)) in memory and length 3"
              testCase "Empty vec on empty matrix"
              <| fun _ ->
                  let vec = SparseVector [||]
                  let matrix = SparseMatrix <| array2D []
                  let Result = MultiplyVecMat vec matrix fAdd fMult

                  Expect.equal
                  <| (Result.Memory, Result.Length)
                  <| (None, 0u)
                  <| "Multiply empty SparseVector on empty SparseMatrix should be SparseVector with None in memory and length 0"
              testCase "Vec with length 1 on matrix with 1 column and line"
              <| fun _ ->
                  let vec = SparseVector [| Some 1 |]
                  let matrix = SparseMatrix <| array2D [ [ Some 2 ] ]
                  let Result = MultiplyVecMat vec matrix fAdd fMult

                  Expect.equal
                  <| (Result.Memory, Result.Length)
                  <| (Leaf 2, 1u)
                  <| "Multiply SparseVector from [|Some 1|] on SparseMatrix from [[Some 2]] should be SparseVector with Leaf 2 in memory and length 1" ]

module SomePropertyTests =
    [<Tests>]
    let tests =
        testList
            "Some property tests"
            [ testProperty "Lengths of separated vectors"
              <| fun (arr: array<int option>) ->
                  let length =
                      if arr.Length = 1 then
                          2u
                      else
                          ClosestDegreeOf2(uint arr.Length) 0u

                  let vec = Vector(arr, 0u, length)
                  let Result = Separator vec

                  Expect.equal
                  <| (fst Result).Length + (snd Result).Length
                  <| length
                  <| "Sum of lengths of separated vectors should be equal to length of main vector"
              testProperty "Item from the cell of array is equal to item from the cell of SparseVector"
              <| fun (arr: array<int option>) ->
                  let Result = SparseVector arr
                  let r = Random().Next(0, arr.Length)

                  if arr.Length = 0 then
                      skiptest |> ignore
                  else
                      Expect.equal
                      <| arr[r]
                      <| Result[uint r]
                      <| "Item from the cell of array should be equal to item from the cell of SparseVector"
              testProperty "Columns and lines of separated matrix"
              <| fun (arr: int option [,]) ->
                  let length =
                      if Array2D.length2 arr = 1 || Array2D.length1 arr = 1 then
                          max
                          <| 2u
                          <| (ClosestDegreeOf2
                              <| uint (Array2D.length2 arr)
                              <| uint (Array2D.length1 arr))
                      else
                          ClosestDegreeOf2
                          <| uint (Array2D.length2 arr)
                          <| uint (Array2D.length1 arr)

                  let matrix = Matrix(arr, (0u, 0u), length, length)
                  let x, y, z, w = SparseMatrix.Separator matrix

                  Expect.equal
                  <| (x.Columns + w.Columns, y.Lines + z.Lines)
                  <| (length, length)
                  <| "Sum of columns and lines of separated matrices should be equal to columns and lines of main matrix"
              testProperty "Item from the cell of array2D is equal to item from the cell of SparseMatrix"
              <| fun (arr: int option [,]) ->
                  let Result = SparseMatrix arr
                  let x = Random().Next(0, Array2D.length2 arr)
                  let y = Random().Next(0, Array2D.length1 arr)

                  if Array2D.length1 arr = 0 || Array2D.length2 arr = 0 then
                      skiptest |> ignore
                  else
                      Expect.equal
                      <| arr[y, x]
                      <| Result[uint x, uint y]
                      <| "Item from the cell of array2D should be equal to item from the cell of SparseMatrix" ]

module SpecialPropertyTests =
    let GeneratorOfVectors (length: int) =
        let arr = Array.init (abs length) (fun _ -> Random().Next(1, 10))

        let arrOfSome =
            arr
            |> Array.map (fun x -> if x > 2 then Option.None else Some(x))

        SparseVector arrOfSome, arrOfSome

    let GeneratorOfMatrix (length: int) =
        let arr2D = Array2D.init (abs length) (abs length) (fun _ _ -> Random().Next(1, 10))

        let arrOfSome2D =
            arr2D
            |> Array2D.map (fun x -> if x > 2 then Option.None else Some(x))

        SparseMatrix arrOfSome2D, arrOfSome2D

    let rec BinaryTreeInspector tree =
        match tree with
        | Node (None, None) -> false
        | Node (left, right) ->
            BinaryTreeInspector left
            && BinaryTreeInspector right
        | _ -> true

    [<Tests>]
    let tests =
        testList
            "Some multiply property tests"
            [ testProperty "Sum of two vectors"
              <| fun (length: int) ->
                  let vec1, arrOfSome1 = GeneratorOfVectors(abs length)
                  let vec2, arrOfSome2 = GeneratorOfVectors(abs length)
                  let Result = FAddVector MultiplyTests.fAdd vec1 vec2

                  let NaiveSum (arr1: array<int option>) (arr2: array<int option>) =
                      let arrOfSum = Array.zeroCreate arr1.Length

                      for i in 0 .. (abs length) - 1 do
                          arrOfSum[i] <- MultiplyTests.fAdd arr1[i] arr2[i]

                      arrOfSum

                  Expect.equal Result.Memory
                  <| SparseVector(
                      NaiveSum arrOfSome1 arrOfSome2
                  )
                      .Memory
                  <| "Results of FAddTree with two vectors should be the same with naive sum"

                  Expect.equal
                  <| BinaryTreeInspector Result.Memory
                  <| true
                  <| "Tree is unreduced"
              testProperty "Multiply vec on matrix"
              <| fun (length: int) ->

                  let vec, arrOfSome = GeneratorOfVectors(abs length)
                  let matrix, arrOfSome2D = GeneratorOfMatrix(abs length)
                  let Result = MultiplyVecMat vec matrix MultiplyTests.fAdd MultiplyTests.fMult

                  let NaiveMultiply (arr: array<int option>) (arr2D: int option [,]) =
                      let arrOfMult = Array.zeroCreate (Array2D.length2 arr2D)

                      for i in 0 .. Array2D.length2 arr2D - 1 do
                          for j in 0 .. (abs length) - 1 do
                              arrOfMult[i] <- MultiplyTests.fAdd arrOfMult[i] (MultiplyTests.fMult arr[j] arr2D[j, i])

                      arrOfMult

                  Expect.equal Result.Memory
                  <| SparseVector(
                      NaiveMultiply arrOfSome arrOfSome2D
                  )
                      .Memory
                  <| "Results of MultiplyVecMat with vector and matrix should be the same with naive multiply"

                  Expect.equal
                  <| BinaryTreeInspector Result.Memory
                  <| true
                  <| "Tree is unreduced" ]
