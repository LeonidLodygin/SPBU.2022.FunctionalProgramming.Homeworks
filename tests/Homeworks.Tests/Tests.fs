namespace Homeworks.Tests

open Expecto

module SayTests =
    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "1 in power of n"
              <| fun _ ->
                  let result = FirstHomework.SimplePow 1 10000
                  Expect.equal result 1 "1 to the power of n is 1"
              testCase "0 to the power of n"
              <| fun _ ->
                  let result = FirstHomework.SimplePow 0 10
                  Expect.equal result 0 "0 to the power of n != 0 is 0"
              testCase "x to the power of -n"
              <| fun _ ->
                  let result = FirstHomework.SimplePow 4 -2
                  Expect.equal result 0.0625 "4 to the power of -2 is 0.0625"
              testCase "positive to the power of positive"
              <| fun _ ->
                  let result = FirstHomework.SimplePow 2 4
                  Expect.equal result 16.0 "2 to the power of 4 is 16"
              testCase "1 in power of n (FastPow)"
              <| fun _ ->
                  let result = FirstHomework.FastPow 1 10000
                  Expect.equal result 1 "1 to the power of n is 1"
              testCase "0 to the power of n (FastPow)"
              <| fun _ ->
                  let result = FirstHomework.FastPow 0 10
                  Expect.equal result 0 "0 to the power of n != 0 is 0"
              testCase "x to the power of -n (FastPow)"
              <| fun _ ->
                  let result = FirstHomework.FastPow 4 -2
                  Expect.equal result 0.0625 "4 to the power of -2 is 0.0625"
              testCase "positive to the power of positive (FastPow)"
              <| fun _ ->
                  let result = FirstHomework.FastPow 2 4
                  Expect.equal result 16.0 "2 to the power of 4 is 16"
              //float
              testCase "array [|5.5; 5.5; 5.5; 5.5; 5.5; 5.5; 5.5|]"
              <| fun _ ->
                  let (result: float) =
                      FirstHomework.MinMaxFromArray [| 5.5
                                                       5.5
                                                       5.5
                                                       5.5
                                                       5.5
                                                       5.5
                                                       5.5 |]

                  Expect.equal result 0 "For array [|5; 5; 5; 5; 5; 5; 5|] answer is 0"
              //int
              testCase "array [|121; 20; 3; -102; 54; 6; -78|]"
              <| fun _ ->
                  let result =
                      FirstHomework.MinMaxFromArray [| 121
                                                       20
                                                       3
                                                       -102
                                                       54
                                                       6
                                                       -78 |]

                  Expect.equal result 223 "For array [|121; 20; 3; -102; 54; 6; -78|] answer is 223"
              //byte
              testCase "array [|37uy;121uy;10uy|]"
              <| fun _ ->
                  let (result: byte) =
                      FirstHomework.MinMaxFromArray [| 37uy
                                                       121uy
                                                       10uy |]

                  Expect.equal result 111uy "For array [|37uy;121uy;10uy|] answer is 111uy"
              //uint16
              testCase "array [|371us;1231us;1001us|]"
              <| fun _ ->
                  let (result: uint16) =
                      FirstHomework.MinMaxFromArray [| 371us
                                                       1231us
                                                       1001us |]

                  Expect.equal result 860us "For array [|371us;1231us;1001us|] answer is 861us"
              //decimal
              testCase "array [|13.6m;121m;19.653m;152.125m|]"
              <| fun _ ->
                  let (result: decimal) =
                      FirstHomework.MinMaxFromArray [| 13.6m
                                                       121m
                                                       19.653m
                                                       152.125m |]

                  Expect.equal result 138.525m "For array [|13.6m;121m;19.653m;152.125m|] answer is 861us"
              testCase "Odds between 0 and 1"
              <| fun _ ->
                  let result = FirstHomework.OddNumbersArray 0 1
                  Expect.equal result [||] "Zero odds between 0 and 1"
              testCase "Odds between "
              <| fun _ ->
                  let result = FirstHomework.OddNumbersArray -5 2
                  Expect.equal result [| -3; -1; 1 |] "Odds between -5 and 2: (-3; -1; 1)" ]
