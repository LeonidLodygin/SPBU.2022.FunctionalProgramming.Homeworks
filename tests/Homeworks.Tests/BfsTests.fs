module Homeworks.BfsTests


open Expecto
open Microsoft.FSharp.Core
open SparseVector
open BreadthFirstSearch



module SimpleTests =
    [<Tests>]
    let tests =
        testList
            "Some tests for vector"
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
