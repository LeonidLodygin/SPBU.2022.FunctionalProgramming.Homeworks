namespace Homeworks.Tests

open Expecto
open FsCheck
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
                  let Result = MyListHomework.Concatenation Empty Empty
                  Expect.equal Result Empty "Empty list with empty list -> empty list"
              testCase "Some MyList with some MyList"
              <| fun _ ->
                  let Result =
                      MyListHomework.Concatenation
                      <| Cons(3, Cons(0, Cons(1, Empty)))
                      <| Cons(2, Cons(14, Cons(10, Empty)))

                  Expect.equal
                  <| Result
                  <| Cons(3, Cons(0, Cons(1, Cons(2, Cons(14, Cons(10, Empty))))))
                  <| "Result should be: Cons(3, Cons(0, Cons(1, Cons(2, Cons(14, Cons(10, Empty))))))"
              testCase "Empty ILIst with empty IList"
              <| fun _ ->
                  let Result =
                      Concatenation
                      <| MyOOPEmptyList()
                      <| MyOOPEmptyList()

                  Expect.equal
                  <| Result
                  <| MyOOPEmptyList()
                  <| "Empty IList with empty IList -> empty IList"
              testCase "Some ILIst with some IList"
              <| fun _ ->
                  let Result =
                      Concatenation
                      <| MyOOPNonEmptyList(1, MyOOPNonEmptyList(2, MyOOPNonEmptyList(3, MyOOPEmptyList())))
                      <| MyOOPNonEmptyList(4, MyOOPNonEmptyList(5, MyOOPNonEmptyList(6, MyOOPEmptyList())))

                  Expect.equal
                  <| Result
                  <| MyOOPNonEmptyList(
                      1,
                      MyOOPNonEmptyList(
                          2,
                          MyOOPNonEmptyList(
                              3,
                              MyOOPNonEmptyList(4, MyOOPNonEmptyList(5, MyOOPNonEmptyList(6, MyOOPEmptyList())))
                          )
                      )
                  )
                  <| "Empty IList with empty IList -> empty IList"
              // Some easy tests of BubbleSort
              testCase "BubbleSort of some MyList"
              <| fun _ ->
                  let Result = MyListHomework.BubbleSort(Cons(10, Cons(9, Cons(8, Cons(7, Empty)))))

                  Expect.equal
                  <| Result
                  <| Cons(7, Cons(8, Cons(9, Cons(10, Empty))))
                  <| "Result should be: Cons(7, Cons(8, Cons(9, Cons(10, Empty))))"
              testCase "BubbleSort of empty MyList"
              <| fun _ ->
                  let Result = MyListHomework.BubbleSort(Empty)

                  Expect.equal
                  <| Result
                  <| Empty
                  <| "Result should be: Empty"
              testCase "BubbleSort of some ILIst"
              <| fun _ ->
                  let Result =
                      BubbleSort(
                          MyOOPNonEmptyList(
                              10,
                              MyOOPNonEmptyList(
                                  9,
                                  MyOOPNonEmptyList(
                                      8,
                                      MyOOPNonEmptyList(7, MyOOPNonEmptyList(6, MyOOPNonEmptyList(5, MyOOPEmptyList())))
                                  )
                              )
                          )
                      )

                  Expect.equal
                  <| OOPListToMyList(Result)
                  <| Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Cons(10, Empty))))))
                  <| "Result should be: Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Cons(10, Empty))))))"
              testCase "BubbleSort of empty IList"
              <| fun _ ->
                  let Result = BubbleSort(MyOOPEmptyList())

                  Expect.equal
                  <| Result
                  <| MyOOPEmptyList()
                  <| "BubbleSort of MyOOPEmptyList should be: MyOOPEmptyList"
              // Some easy tests of QuickSort
              testCase "QuickSort of some MyList"
              <| fun _ ->
                  let Result = MyListHomework.QuickSort(Cons(10, Cons(9, Cons(8, Cons(7, Empty)))))

                  Expect.equal
                  <| Result
                  <| Cons(7, Cons(8, Cons(9, Cons(10, Empty))))
                  <| "Result should be: Cons(7, Cons(8, Cons(9, Cons(10, Empty))))"
              testCase "BubbleSort of empty MyList"
              <| fun _ ->
                  let Result = MyListHomework.QuickSort(Empty)

                  Expect.equal
                  <| Result
                  <| Empty
                  <| "Result should be: Empty"
              testCase "BubbleSort of some ILIst"
              <| fun _ ->
                  let Result =
                      QuickSort(
                          MyOOPNonEmptyList(
                              10,
                              MyOOPNonEmptyList(
                                  9,
                                  MyOOPNonEmptyList(
                                      8,
                                      MyOOPNonEmptyList(7, MyOOPNonEmptyList(6, MyOOPNonEmptyList(5, MyOOPEmptyList())))
                                  )
                              )
                          )
                      )

                  Expect.equal
                  <| OOPListToMyList(Result)
                  <| Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Cons(10, Empty))))))
                  <| "Result should be: Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Cons(10, Empty))))))"
              testCase "BubbleSort of empty IList"
              <| fun _ ->
                  let Result = QuickSort(MyOOPEmptyList())

                  Expect.equal
                  <| Result
                  <| MyOOPEmptyList()
                  <| "BubbleSort of MyOOPEmptyList should be: MyOOPEmptyList"
              // Some fscheck tests
              ]
