module Homeworks.BfsTests

open System
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
                  let Result = VecFromList [ 0, true ] 1

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
                  let Result = VecFromList [ 1, true; 4, true; 5, true ] 6

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
                          [ 0, true ]
                          4

                  Expect.equal Result.Memory
                  <| Node(Node(Leaf 0, Leaf 1), Node(None, Leaf 2))
                  <| "Bfs should return \"Node(Node(Leaf 0, Leaf 1), Node(None, Leaf 2))\" from [(0, 1, Some 4);(1, 0, Some 4);(1, 3, Some 9); (3, 1, Some 9)] and start position in [0]" ]

module PropertyTests =
    [<Tests>]
    let tests =
        testList
            "Some property tests"
            [ testProperty "SparseVector from coordinates"
              <| fun (list: List<int>) (length: int) ->
                  let size = (abs length) + 1

                  let ResultList =
                      List.map (fun value -> abs value % size, true) list
                      |> List.distinct

                  let vector1 = VecFromList ResultList size
                  let arr = Array.create size Option.None

                  for i in ResultList do
                      arr[fst i] <- Some true

                  let vector2 = SparseVector arr

                  Expect.equal
                  <| vector1.Memory
                  <| vector2.Memory
                  <| "VecFromList should give the same result with SparseVector from array with the same values"

              testProperty "SparseMatrix from coordinates"
              <| fun (list: List<int * int>) (length: int) ->
                  let size = (abs length) + 1

                  let ResultList =
                      List.map (fun (x, y) -> (abs x % size, abs y % size)) list
                      |> List.distinct

                  let arr = Array2D.create size size Option.None

                  let rec Helper list =
                      match list with
                      | [] -> []
                      | (x, y) :: tl ->
                          let value = Random().Next(1, 10)
                          arr[x, y] <- Some(Some value)
                          (x, y, Some value) :: (Helper tl)

                  let finalList = Helper ResultList
                  let matrix1 = MatrixFromList finalList size
                  let matrix2 = SparseMatrix(arr)

                  Expect.equal
                  <| matrix1.Memory
                  <| matrix2.Memory
                  <| "Something wrong with SparseMatrix from list"

              testProperty "Bfs against naive bfs"
              <| fun (list: List<int * int>) (length: int) ->
                  if list.Length = 0 then
                      skiptest |> ignore
                  else
                      let size = (abs length) + 1

                      let ResultList =
                          List.map (fun (x, y) -> (abs x % size, abs y % size, Some 10)) list
                          |> List.distinct

                      let start = [ first ResultList.Head ]
                      let Result1 = Bfs ResultList [ first ResultList.Head, true ] size

                      let arr = Array2D.create size size Option.None

                      for i in ResultList do
                          let x = first i
                          let y = second i
                          arr[x, y] <- third i

                      let naiveBFS (start: int list) (arr: 'a option [,]) =
                          let queue = start |> List.map (fun x -> (x, 0))

                          let QueueFormatter queue apex iter =
                              let rec inner list counter =
                                  if counter < 0 then
                                      list
                                  else
                                      let value = arr[apex, counter]

                                      if value = Option.None then
                                          inner list (counter - 1)
                                      else
                                          inner (list @ [ counter, iter ]) (counter - 1)

                              inner queue (Array2D.length2 arr - 1)

                          let rec helper queue result visited =
                              match queue with
                              | [] -> result
                              | (apex, iter) :: tl ->
                                  if List.contains apex visited then
                                      helper tl result visited
                                  else
                                      let visited = apex :: visited
                                      let newQ = QueueFormatter tl apex (iter + 1)
                                      helper newQ (result @ [ (apex, iter) ]) visited

                          if queue.IsEmpty then
                              []
                          else
                              helper queue [] []

                      let list = naiveBFS start arr
                      let Result2 = VecFromList list size

                      Expect.equal
                      <| Result1.Memory
                      <| Result2.Memory
                      <| "Something wrong with bfs" ]
