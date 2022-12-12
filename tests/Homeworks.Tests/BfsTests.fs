module Homeworks.BfsTests


open Expecto
open Microsoft.FSharp.Core
open SparseMatrix
open SparseVector
open BreadthFirstSearch

module SimpleTests =
    [<Tests>]
    let tests =
        testList
            "Some simple tests"
            [ testCase "Front from list with 1 apex"
              <| fun _ ->
                  let Result = VecFromList [ 0 ] 1

                  Expect.equal
                  <| Result.Memory
                  <| Leaf true
                  <| "VecFromList should return \"Leaf true\" from list [0] and size 1."
              testCase "Front from empty list"
              <| fun _ ->
                  let Result = VecFromList [] 0

                  Expect.equal
                  <| Result.Memory
                  <| BinaryTree.None
                  <| "VecFromList should return \"BinaryTree.None\" from empty list"
              testCase "Front from non empty list"
              <| fun _ ->
                  let Result = VecFromList [ 1; 4; 5 ] 6

                  Expect.equal
                  <| Result.Memory
                  <| Node(Node(Node(None, Leaf true), None), Node(Node(Leaf true, Leaf true), None))
                  <| "VecFromList should return \"Node(Node(Node(None, Leaf true), None), Node(Node(Leaf true, Leaf true), None))\" from [1; 4; 5] list with size 6"
              testCase "Matrix from empty list"
              <| fun _ ->
                  let Result = MatrixFromList [] 0

                  Expect.equal
                  <| Result.Memory
                  <| QuadTree.None
                  <| "Matrix from empty list should be \"QuadTree.None\""
              testCase "Matrix from non empty list"
              <| fun _ ->
                  let Result =
                      MatrixFromList
                          [ (0, 1, Some 4)
                            (1, 0, Some 4)
                            (1, 3, Some 9)
                            (3, 1, Some 9) ]
                          4

                  Expect.equal
                  <| Result.Memory
                  <| QuadTree.Node(
                      QuadTree.Node(QuadTree.None, QuadTree.Leaf(Some 4), QuadTree.Leaf(Some 4), QuadTree.None),
                      QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.Leaf(Some 9)),
                      QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.Leaf(Some 9)),
                      QuadTree.None
                  )
                  <| "Matrix from [(0, 1, Some 4);(1, 0, Some 4);(1, 3, Some 9); (3, 1, Some 9)]  should be \"QuadTree.Node (QuadTree.Node (QuadTree.None, QuadTree.Leaf (Some 4), QuadTree.Leaf (Some 4), QuadTree.None), QuadTree.Node (QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.Leaf (Some 9)), QuadTree.Node (QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.Leaf (Some 9)), QuadTree.None)\""
              testCase "Bfs with some graph and apexes"
              <| fun _ ->
                  let Result =
                      Bfs
                          [ (0, 1, Some 4)
                            (1, 0, Some 4)
                            (1, 3, Some 9)
                            (3, 1, Some 9) ]
                          [ 0 ]
                          4

                  Expect.equal Result.Memory
                  <| Node(Node(Leaf 0, Leaf 1), Node(None, Leaf 2))
                  <| "Bfs should return \"Node(Node(Leaf 0, Leaf 1), Node(Leaf 2, None))\" from [(0, 1, Some 4);(1, 0, Some 4);(1, 3, Some 9); (3, 1, Some 9)] and start position in [0]" ]

module PropertyTests =
    [<Tests>]
    let tests =
        testList
            "Some property tests"
            [ testProperty "SparseVector from coordinates"
              <| fun (list: List<int>) (length: int) ->
                  let size = (abs length) + 1

                  let ResultList =
                      List.map (fun value -> abs value % size) list
                      |> List.distinct

                  let vector1 = VecFromList ResultList size
                  let arr = Array.create size Option.None

                  for i in ResultList do
                      arr[i] <- Some true

                  let vector2 = SparseVector arr

                  Expect.equal
                  <| vector1.Memory
                  <| vector2.Memory
                  <| "Error" ]
