module Homeworks.BfsTests

open System
open Expecto
open SparseMatrix
open SparseVector
open BreadthFirstSearch
open GraphBuild

module SimpleTests =
    [<Tests>]
    let tests =
        testList
            "Some simple tests"
            [ testCase "Matrix from empty list"
              <| fun _ ->
                  let result = MatrixFromList [] 0u

                  Expect.equal
                  <| result
                  <| QuadTree.None
                  <| "Matrix from empty list should be \"QuadTree.None\""
              testCase "Matrix from non empty list"
              <| fun _ ->
                  let result =
                      MatrixFromList
                          [ (0u, 1u, Some 4)
                            (1u, 0u, Some 4)
                            (1u, 3u, Some 9)
                            (3u, 1u, Some 9) ]
                          4u

                  Expect.equal
                  <| result
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
                      [ (0u, 1u, Some 4)
                        (1u, 0u, Some 4)
                        (1u, 3u, Some 9)
                        (3u, 1u, Some 9) ]

                  let matrix = SparseMatrix(list, 4u, 4u)
                  let graph = Graph(matrix, 4u, 2u)
                  let result = Bfs graph [ 0u ] 0 0

                  Expect.equal result.Memory
                  <| Node(Node(Leaf 0u, Leaf 1u), Node(None, Leaf 2u))
                  <| "Bfs should return \"Node(Node(Leaf 0, Leaf 1), Node(None, Leaf 2))\" from [(0, 1, Some 4);(1, 0, Some 4);(1, 3, Some 9); (3, 1, Some 9)] and start position in [0]"
              testCase "Bfs with some graph and zero apexes"
              <| fun _ ->
                  let list =
                      [ (0u, 1u, Some 4)
                        (1u, 0u, Some 4)
                        (1u, 3u, Some 9)
                        (3u, 1u, Some 9) ]

                  let matrix = SparseMatrix(list, 4u, 4u)
                  let graph = Graph(matrix, 4u, 2u)
                  let result = Bfs graph [] 0 0

                  Expect.equal result.Memory
                  <| None
                  <| "Bfs should return \"BinaryTree.None\" from [(0, 1, Some 4);(1, 0, Some 4);(1, 3, Some 9); (3, 1, Some 9)] and zero start positions"
              testCase "Bfs with some graph and all apexes"
              <| fun _ ->
                  let list =
                      [ (0u, 1u, Some 4)
                        (1u, 0u, Some 4)
                        (1u, 3u, Some 9)
                        (3u, 1u, Some 9) ]

                  let matrix = SparseMatrix(list, 4u, 4u)
                  let graph = Graph(matrix, 4u, 2u)
                  let result = Bfs graph [ 0u; 1u; 2u; 3u ] 0 0

                  Expect.equal result.Memory
                  <| Node(Node(Leaf 0u, Leaf 0u), Node(Leaf 0u, Leaf 0u))
                  <| "Bfs should return \"Node(Node(Leaf 0, Leaf 0),Node(Leaf 0, Leaf 0))\" from [(0, 1, Some 4);(1, 0, Some 4);(1, 3, Some 9); (3, 1, Some 9)] and start position in [0, 1, 2, 3]"
              testCase "Bfs with empty graph and zero apexes"
              <| fun _ ->
                  let list = []
                  let matrix = SparseMatrix(list, 0u, 0u)
                  let graph = Graph(matrix, 0u, 0u)
                  let result = Bfs graph [] 0 0

                  Expect.equal result.Memory
                  <| None
                  <| "Bfs should return \"None\" from empty graph and zero start positions" ]

module PropertyTests =
    [<Tests>]
    let tests =
        testList
            "Some property tests"
            [ testProperty "SparseMatrix from coordinates"
              <| fun (list: List<uint * uint>) ->
                  let size =
                      if list.IsEmpty then
                          0u
                      else
                          let x = List.maxBy fst list
                          let y = List.maxBy snd list
                          (max (fst x) (snd y)) + 1u

                  let resultList = list |> List.distinct

                  let iSize =
                      try
                          Convert.ToInt32(size)
                      with
                      | :? OverflowException -> failwith $"%A{size} is outside the range of the Int32 type."

                  let arr = Array2D.create iSize iSize Option.None

                  let rec helper (list: List<uint * uint>) =
                      match list with
                      | [] -> []
                      | (x, y) :: tl ->
                          let value = Random().Next(1, 10)

                          let fHead =
                              try
                                  Convert.ToInt32(x)
                              with
                              | :? OverflowException -> failwith $"%A{x} is outside the range of the Int32 type."

                          let sHead =
                              try
                                  Convert.ToInt32(y)
                              with
                              | :? OverflowException -> failwith $"%A{y} is outside the range of the Int32 type."

                          arr[fHead, sHead] <- Some(Some value)
                          (x, y, Some value) :: (helper tl)

                  let finalList = helper resultList
                  let matrix1 = MatrixFromList finalList size
                  let matrix2 = SparseMatrix(arr)

                  Expect.equal
                  <| matrix1
                  <| matrix2.Memory
                  <| "Something wrong with SparseMatrix from list"

              testProperty "Bfs against naive bfs"
              <| fun (list: List<uint * uint>) ->
                  let size =
                      if list.IsEmpty then
                          0u
                      else
                          let x = List.maxBy fst list
                          let y = List.maxBy snd list
                          (max (fst x) (snd y)) + 1u

                  let resultList =
                      List.map (fun (x, y) -> (x, y, Some 10)) list
                      |> List.distinct

                  let start =
                      if resultList.Length <> 0 then
                          [ First resultList.Head ]
                      else
                          []

                  let matrix = SparseMatrix(resultList, size, size)
                  let graph = Graph(matrix, size, uint list.Length)

                  let result1 =
                      Bfs
                          graph
                          (if resultList.Length <> 0 then
                               [ First resultList.Head ]
                           else
                               [])
                          0
                          0

                  let iSize =
                      try
                          Convert.ToInt32(size)
                      with
                      | :? OverflowException -> failwith $"%A{size} is outside the range of the Int32 type."

                  let arr = Array2D.create iSize iSize Option.None

                  for i in resultList do
                      let x = First i
                      let y = Second i

                      let fCoord =
                          try
                              Convert.ToInt32(x)
                          with
                          | :? OverflowException -> failwith $"%A{x} is outside the range of the Int32 type."

                      let sCoord =
                          try
                              Convert.ToInt32(y)
                          with
                          | :? OverflowException -> failwith $"%A{y} is outside the range of the Int32 type."

                      arr[fCoord, sCoord] <- Third i

                  let naiveBFS start (arr: 'A option [,]) =
                      let queue = System.Collections.Generic.Queue<uint * uint>()

                      for i in start do
                          queue.Enqueue(i, 0u)

                      let rec helper result visited =
                          if queue.Count = 0 then
                              result
                          else
                              let x = queue.Dequeue()

                              if Set.contains (fst x) visited then
                                  helper result visited
                              else
                                  let iApex =
                                      try
                                          Convert.ToInt32(fst x)
                                      with
                                      | :? OverflowException ->
                                          failwith $"%A{fst x} is outside the range of the Int32 type."

                                  for i in 0 .. Array2D.length2 arr - 1 do
                                      let value = arr[iApex, i]

                                      if value = Option.None then
                                          ()
                                      else
                                          queue.Enqueue(uint i, snd x + 1u)

                                  helper (result @ [ x ]) (visited.Add(fst x))

                      if queue.Count = 0 then
                          []
                      else
                          helper [] Set.empty

                  let list = naiveBFS start arr
                  let answers = Array.create (int size) Option.None

                  for i in 0 .. list.Length - 1 do
                      answers[int (fst list[i])] <- Some(snd list[i])

                  let result2 = SparseVector(answers)

                  Expect.equal
                  <| result1.Memory
                  <| result2.Memory
                  <| "Something wrong with bfs" ]
