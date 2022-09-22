namespace Homeworks.Tests

open Expecto
open Homeworks

module SayTests =
    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "1 in power of n"
              <| fun _ ->
                  let Result = Main.SimplePow 1 10000
                  Expect.equal Result 1 "1 to the power of n is 1"
              testCase "0 to the power of n"
              <| fun _ ->
                  let Result = Main.SimplePow 0 10
                  Expect.equal Result 0 "0 to the power of n != 0 is 0"
              testCase "x to the power of -n"
              <| fun _ ->
                  let Result = Main.SimplePow 4 -2
                  Expect.equal Result 0.0625 "4 to the power of -2 is 0.0625"
              testCase "0 to the power of 0"
              <| fun _ ->
                  let Result = Main.SimplePow 0 0
                  Expect.equal Result 0.0 "0 to the power of 0 is undefined"
              testCase "positive to the power of positive"
              <| fun _ ->
                  let Result = Main.SimplePow 2 4
                  Expect.equal Result 16.0 "2 to the power of 4 is 16"
              testCase "1 in power of n (FastPow)"
              <| fun _ ->
                  let Result = Main.FastPow 1 10000
                  Expect.equal Result 1 "1 to the power of n is 1"
              testCase "0 to the power of n (FastPow)"
              <| fun _ ->
                  let Result = Main.FastPow 0 10
                  Expect.equal Result 0 "0 to the power of n != 0 is 0"
              testCase "x to the power of -n (FastPow)"
              <| fun _ ->
                  let Result = Main.FastPow 4 -2
                  Expect.equal Result 0.0625 "4 to the power of -2 is 0.0625"
              testCase "0 to the power of 0 (FastPow)"
              <| fun _ ->
                  let Result = Main.FastPow 0 0
                  Expect.equal Result 0.0 "0 to the power of 0 is undefined"
              testCase "positive to the power of positive (FastPow)"
              <| fun _ ->
                  let Result = Main.FastPow 2 4
                  Expect.equal Result 16.0 "2 to the power of 4 is 16"
              testCase "array [|5; 5; 5; 5; 5; 5; 5|]"
              <| fun _ ->
                  let Result =
                      Main.MinMaxFromArray [| 5
                                              5
                                              5
                                              5
                                              5
                                              5
                                              5 |]

                  Expect.equal Result 0 "For array [|5; 5; 5; 5; 5; 5; 5|] answer is 0"
              testCase "array [|121; 20; 3; -102; 54; 6; -78|]"
              <| fun _ ->
                  let Result =
                      Main.MinMaxFromArray [| 121
                                              20
                                              3
                                              -102
                                              54
                                              6
                                              -78 |]

                  Expect.equal Result 223 "For array [|121; 20; 3; -102; 54; 6; -78|] answer is 223"
              testCase "Odds between 0 and 1"
              <| fun _ ->
                  let Result = Main.OddNumbersArray 0 1
                  Expect.equal Result [||] "Zero odds between 0 and 1"
              testCase "Odds between "
              <| fun _ ->
                  let Result = Main.OddNumbersArray -5 2
                  Expect.equal Result [| -3; -1; 1 |] "Odds between -5 and 2: (-3; -1; 1)" ]
