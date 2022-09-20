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
                  Expect.equal Result 0.0 "0 to the power of 0 is undefined" ]
