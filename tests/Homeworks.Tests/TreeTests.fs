﻿namespace Homeworks.Tests

open Expecto
open Microsoft.FSharp.Core
open MyListHomework
open TreeHomework

module TreeTests =
    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Number of different values in tree with 1 node"
              <| fun _ ->
                  let result = NumOfDifferentValues(Leaf(134))
                  Expect.equal result 1 "Tree with 1 node has 1 value"
              testCase "Number of different values in tree with the same values"
              <| fun _ ->
                  let result =
                      NumOfDifferentValues(
                          Node(
                              "ha",
                              [| Node("ha", [| Leaf("ha") |])
                                 Node("ha", [| Leaf("ha") |])
                                 Leaf("ha")
                                 Node("ha", [| Node("ha", [| Leaf("ha") |]) |]) |]
                          )
                      )

                  Expect.equal result 1 "Tree with the same values has only 1 value"
              testCase "Number of different values in tree with int values"
              <| fun _ ->
                  let result =
                      NumOfDifferentValues(
                          Node(
                              12,
                              [| Node(1, [| Leaf(12) |])
                                 Node(3, [| Leaf(12) |])
                                 Node(10, [| Node(4, [| Leaf(12) |]) |])
                                 Node(15, [| Leaf(12); Leaf(1) |]) |]
                          )
                      )

                  Expect.equal
                      result
                      6
                      "Tree: Node(12, [|Node(1, [|Leaf(12)|]); Node(3, [|Leaf(12)|]); Node(10, [|Node(4, [|Leaf(12)|])|]); Node(15, [|Leaf(12);Leaf(1)|])|]) has 6 different values"
              testCase "Number of different values in tree with string values"
              <| fun _ ->
                  let result =
                      NumOfDifferentValues(
                          Node(
                              "12",
                              [| Node("1", [| Leaf("12") |])
                                 Node("3", [| Leaf("12") |])
                                 Node("10", [| Node("4", [| Leaf("12") |]) |])
                                 Node("15", [| Leaf("12"); Leaf("1") |]) |]
                          )
                      )

                  Expect.equal
                      result
                      6
                      "Tree: Node(\"12\", [|Node(\"1\", [|Leaf(\"12\")|]); Node(\"3\", [|Leaf(\"12\")|]); Node(\"10\", [|Node(\"4\", [|Leaf(\"12\")|])|]); Node(\"15\", [|Leaf(\"12\");Leaf(\"1\")|])|]) has 6 different values"
              testCase "MyList of tree with 1 node"
              <| fun _ ->
                  Expect.equal
                  <| MyListOfTree(Leaf(134))
                  <| Cons(134, Empty)
                  <| "MyList of tree with 1 node should be Cons(value, Empty)"
              testCase "MyList of some tree"
              <| fun _ ->
                  let result =
                      MyListOfTree(
                          Node(
                              12,
                              [| Node(1, [| Leaf(12) |])
                                 Node(3, [| Leaf(12) |])
                                 Node(10, [| Node(4, [| Leaf(12) |]) |])
                                 Node(15, [| Leaf(12); Leaf(1) |]) |]
                          )
                      )

                  Expect.equal
                      result
                      (Cons(
                          1,
                          Cons(
                              12,
                              Cons(
                                  15,
                                  Cons(12, Cons(4, Cons(10, Cons(12, Cons(3, Cons(12, Cons(1, Cons(12, Empty))))))))
                              )
                          )
                      ))
                      "MyList of tree: Node(12, [|Node(1, [|Leaf(12)|]); Node(3, [|Leaf(12)|]); Node(10, [|Node(4, [|Leaf(12)|])|]); Node(15, [|Leaf(12);Leaf(1)|])|]) should be Cons(12, Cons(1, Cons(12, Cons(3, Cons(12, Cons(10, Cons(4, Cons(12, Cons(15, Cons(12, Cons(1, Empty)))))))))))"
              testProperty "Compare length of MyList from tree with number of different values in tree"
              <| fun (tree: ArbitraryTree<int>) ->
                  Expect.isGreaterThanOrEqual
                  <| Length(MyListOfTree tree)
                  <| NumOfDifferentValues tree
                  <| "Length of MyList should be greater than or equal to the number of different values"
              testProperty "Number of different values"
              <| fun (tree: ArbitraryTree<int>) ->
                  Expect.isGreaterThan
                  <| NumOfDifferentValues tree
                  <| 0
                  <| "Number of different values in tree should be greater than 0"
              testProperty "Length of MyList from tree"
              <| fun (tree: ArbitraryTree<int>) ->
                  Expect.isGreaterThan
                  <| Length(MyListOfTree tree)
                  <| 0
                  <| "Length of MyList from tree should be greater than 0" ]
