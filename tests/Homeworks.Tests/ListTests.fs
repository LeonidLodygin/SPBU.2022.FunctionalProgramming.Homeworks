﻿namespace Homeworks.Tests

open Expecto
open Microsoft.FSharp.Core
open MyListHomework
open IListHomework



module MyListTests =
    [<Tests>]
    let tests =
        testList
            "samples"
            [
              // Some easy tests of Concatenation
              testCase "Concatenation Empty with Empty"
              <| fun _ ->
                  let result = MyListHomework.Concatenation Empty Empty

                  Expect.equal result Empty "Empty list with empty list -> empty list"
              testCase "Concatenation of some MyList with some MyList"
              <| fun _ ->
                  let result =
                      MyListHomework.Concatenation
                      <| Cons(3, Cons(0, Cons(1, Empty)))
                      <| Cons(2, Cons(14, Cons(10, Empty)))

                  Expect.equal
                  <| result
                  <| Cons(3, Cons(0, Cons(1, Cons(2, Cons(14, Cons(10, Empty))))))
                  <| "Result should be: Cons(3, Cons(0, Cons(1, Cons(2, Cons(14, Cons(10, Empty))))))"
              testCase "Concatenation of empty ILIst with empty IList"
              <| fun _ ->
                  let result = Concatenation <| EmptyList() <| EmptyList()

                  Expect.equal
                  <| OOPListToMyList(result)
                  <| Empty
                  <| "Empty IList with empty IList -> empty IList"
              testCase "Concatenation of some ILIst with some IList"
              <| fun _ ->
                  let result =
                      Concatenation
                      <| List(1, List(2, List(3, EmptyList())))
                      <| List(4, List(5, List(6, EmptyList())))

                  Expect.equal
                  <| OOPListToMyList(result)
                  <| Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Empty))))))
                  <| "Result should be: Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Empty))))))"
              // Some easy tests of BubbleSort
              testCase "BubbleSort of some MyList"
              <| fun _ ->
                  let result = MyListHomework.BubbleSort(Cons(10, Cons(9, Cons(8, Cons(7, Empty)))))

                  Expect.equal
                  <| result
                  <| Cons(7, Cons(8, Cons(9, Cons(10, Empty))))
                  <| "Result should be: Cons(7, Cons(8, Cons(9, Cons(10, Empty))))"
              testCase "BubbleSort of empty MyList"
              <| fun _ ->
                  let result = MyListHomework.BubbleSort(Empty)

                  Expect.equal
                  <| result
                  <| Empty
                  <| "Result should be: Empty"
              testCase "BubbleSort of some ILIst"
              <| fun _ ->
                  let result =
                      BubbleSort(List(10, List(9, List(8, List(7, List(6, List(5, EmptyList())))))))

                  Expect.equal
                  <| OOPListToMyList(result)
                  <| Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Cons(10, Empty))))))
                  <| "Result should be: Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Cons(10, Empty))))))"
              testCase "BubbleSort of empty IList"
              <| fun _ ->
                  let result = BubbleSort(EmptyList())

                  Expect.equal
                  <| OOPListToMyList(result)
                  <| Empty
                  <| "BubbleSort of MyOOPEmptyList should be: MyOOPEmptyList"
              // Some easy tests of QuickSort
              testCase "QuickSort of some MyList"
              <| fun _ ->
                  let result = MyListHomework.QuickSort(Cons(10, Cons(9, Cons(8, Cons(7, Empty)))))

                  Expect.equal
                  <| result
                  <| Cons(7, Cons(8, Cons(9, Cons(10, Empty))))
                  <| "Result should be: Cons(7, Cons(8, Cons(9, Cons(10, Empty))))"
              testCase "QuickSort of empty MyList"
              <| fun _ ->
                  let result = MyListHomework.QuickSort(Empty)

                  Expect.equal
                  <| result
                  <| Empty
                  <| "Result should be: Empty"
              testCase "QuickSort of some ILIst"
              <| fun _ ->
                  let result =
                      QuickSort(List(10, List(9, List(8, List(7, List(6, List(5, EmptyList())))))))

                  Expect.equal
                  <| OOPListToMyList(result)
                  <| Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Cons(10, Empty))))))
                  <| "Result should be: Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Cons(10, Empty))))))"
              testCase "QuickSort of empty IList"
              <| fun _ ->
                  let result = QuickSort(EmptyList())

                  Expect.equal
                  <| OOPListToMyList(result)
                  <| Empty
                  <| "BubbleSort of MyOOPEmptyList should be: MyOOPEmptyList"
              // Some fscheck tests
              testProperty "BubbleSort of MyList<int> doing the same with QuickSort"
              <| fun (lst: MyList<int>) ->
                  Expect.equal
                  <| MyListHomework.BubbleSort(lst)
                  <| MyListHomework.QuickSort(lst)
                  <| "BubbleSort and QuickSort gave different results"
              testProperty "BubbleSort of MyList<string> doing the same with QuickSort"
              <| fun (lst: MyList<string>) ->
                  Expect.equal
                  <| MyListHomework.BubbleSort(lst)
                  <| MyListHomework.QuickSort(lst)
                  <| "BubbleSort and QuickSort gave different results"
              testProperty "BubbleSort of IList<int> doing the same with QuickSort"
              <| fun (lst: MyList<int>) ->
                  Expect.equal
                  <| OOPListToMyList(BubbleSort(MyListToOOPList(lst)))
                  <| OOPListToMyList(QuickSort(MyListToOOPList(lst)))
                  <| "BubbleSort and QuickSort gave different results"
              testProperty "BubbleSort of IList<string> doing the same with QuickSort"
              <| fun (lst: MyList<string>) ->
                  Expect.equal
                  <| OOPListToMyList(BubbleSort(MyListToOOPList(lst)))
                  <| OOPListToMyList(BubbleSort(MyListToOOPList(lst)))
                  <| "BubbleSort and QuickSort gave different results"
              testProperty "Concatenation of MyList<int> doing the same with built-in function"
              <| fun (lst: MyList<int>) (lst2: MyList<int>) ->
                  Expect.equal
                  <| MyListHomework.Concatenation lst lst2
                  <| ListToMyList(MyListToList lst @ MyListToList lst2)
                  <| "Concatenation and built-in function of concatenation gave different results"
              testProperty "Concatenation of MyList<string> doing the same with built-in function"
              <| fun (lst: MyList<string>) (lst2: MyList<string>) ->
                  Expect.equal
                  <| MyListHomework.Concatenation lst lst2
                  <| ListToMyList(MyListToList lst @ MyListToList lst2)
                  <| "Concatenation and built-in function of concatenation gave different results"
              testProperty "Comparing length of concatenation of two lists with sum of lengths of two lists"
              <| fun (lst: MyList<int>) (lst2: MyList<int>) ->
                  Expect.equal
                  <| Length(MyListHomework.Concatenation lst lst2)
                  <| Length lst + Length lst2
                  <| "Length of concatenation should be equal with lengths of two lists" ]
