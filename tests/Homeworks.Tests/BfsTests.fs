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
            [ testCase "Matrix from empty list"
              <| fun _ ->
                  let Result = MatrixFromList [] 0

                  Expect.equal
                  <| Result
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
                  <| Result
                  <| QuadTree.Node(
                      QuadTree.Node(QuadTree.None, QuadTree.Leaf(Some 4), QuadTree.Leaf(Some 4), QuadTree.None),
                      QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.Leaf(Some 9)),
                      QuadTree.Node(QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.Leaf(Some 9)),
                      QuadTree.None
                  )
                  <| "Matrix from [(0, 1, Some 4);(1, 0, Some 4);(1, 3, Some 9); (3, 1, Some 9)]  should be \"QuadTree.Node (QuadTree.Node (QuadTree.None, QuadTree.Leaf (Some 4), QuadTree.Leaf (Some 4), QuadTree.None), QuadTree.Node (QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.Leaf (Some 9)), QuadTree.Node (QuadTree.None, QuadTree.None, QuadTree.None, QuadTree.Leaf (Some 9)), QuadTree.None)\""
              testCase "Bfs with some graph and apexes"
              <| fun _ ->
                  let list =
                      [ (0, 1, Some 4)
                        (1, 0, Some 4)
                        (1, 3, Some 9)
                        (3, 1, Some 9) ]

                  let matrix = SparseMatrix(list, 4)
                  let Result = Bfs matrix [ 0 ]

                  Expect.equal Result.Memory
                  <| Node(Node(Leaf 0, Leaf 1), Node(None, Leaf 2))
                  <| "Bfs should return \"Node(Node(Leaf 0, Leaf 1), Node(None, Leaf 2))\" from [(0, 1, Some 4);(1, 0, Some 4);(1, 3, Some 9); (3, 1, Some 9)] and start position in [0]"
              testCase "Bfs with some graph and zero apexes"
              <| fun _ ->
                  let list =
                      [ (0, 1, Some 4)
                        (1, 0, Some 4)
                        (1, 3, Some 9)
                        (3, 1, Some 9) ]

                  let matrix = SparseMatrix(list, 4)
                  let Result = Bfs matrix []

                  Expect.equal Result.Memory
                  <| None
                  <| "Bfs should return \"BinaryTree.None\" from [(0, 1, Some 4);(1, 0, Some 4);(1, 3, Some 9); (3, 1, Some 9)] and zero start positions"
              testCase "Bfs with some graph and all apexes"
              <| fun _ ->
                  let list =
                      [ (0, 1, Some 4)
                        (1, 0, Some 4)
                        (1, 3, Some 9)
                        (3, 1, Some 9) ]

                  let matrix = SparseMatrix(list, 4)
                  let Result = Bfs matrix [ 0; 1; 2; 3 ]

                  Expect.equal Result.Memory
                  <| Node(Node(Leaf 0, Leaf 0), Node(Leaf 0, Leaf 0))
                  <| "Bfs should return \"Node(Node(Leaf 0, Leaf 0),Node(Leaf 0, Leaf 0))\" from [(0, 1, Some 4);(1, 0, Some 4);(1, 3, Some 9); (3, 1, Some 9)] and start position in [0, 1, 2, 3]"
              testCase "Bfs with empty graph and zero apexes"
              <| fun _ ->
                  let list = []
                  let matrix = SparseMatrix(list, 0)
                  let Result = Bfs matrix []

                  Expect.equal Result.Memory
                  <| None
                  <| "Bfs should return \"None\" from empty graph and zero start positions" ]

module PropertyTests =
    [<Tests>]
    let tests =
        testList
            "Some property tests"
            [ testProperty "SparseMatrix from coordinates"
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
                  <| matrix1
                  <| matrix2.Memory
                  <| "Something wrong with SparseMatrix from list"

              testProperty "Bfs against naive bfs"
              <| fun (list: List<int * int>) (length: int) ->
                  let size = (abs length) + 1

                  let resultList =
                      List.map (fun (x, y) -> (abs x % size, abs y % size, Some 10)) list
                      |> List.distinct

                  let start =
                      if resultList.Length <> 0 then
                          [ First resultList.Head ]
                      else
                          []

                  let matrix = SparseMatrix(resultList, size)

                  let Result1 =
                      Bfs
                          matrix
                          (if resultList.Length <> 0 then
                               [ First resultList.Head ]
                           else
                               [])

                  let arr = Array2D.create size size Option.None

                  for i in resultList do
                      let x = First i
                      let y = Second i
                      arr[x, y] <- Third i

                  let NaiveBFS (start: int list) (arr: 'a option [,]) =
                      let queue = start |> List.map (fun x -> (x, 0))

                      let QueueFormatter queue apex iter =
                          let rec Helper list counter =
                              if counter < 0 then
                                  list
                              else
                                  let value = arr[apex, counter]

                                  if value = Option.None then
                                      Helper list (counter - 1)
                                  else
                                      Helper(list @ [ counter, iter ]) (counter - 1)

                          Helper queue (Array2D.length2 arr - 1)

                      let rec Helper queue result visited =
                          match queue with
                          | [] -> result
                          | (apex, iter) :: tl ->
                              if List.contains apex visited then
                                  Helper tl result visited
                              else
                                  let visited = apex :: visited
                                  let newQ = QueueFormatter tl apex (iter + 1)
                                  Helper newQ (result @ [ (apex, iter) ]) visited

                      if queue.IsEmpty then
                          []
                      else
                          Helper queue [] []

                  let list = NaiveBFS start arr

                  let answers = Array.create size Option.None

                  for i in 0 .. list.Length - 1 do
                      answers[fst list[i]] <- Some(snd list[i])

                  let Result2 = SparseVector(answers)

                  Expect.equal
                  <| Result1.Memory
                  <| Result2.Memory
                  <| "Something wrong with bfs" ]
