namespace Homeworks.Tests

open Expecto
open Homeworks

module SayTests =
    [<Tests>]
    let tests =
        testList
            "samples"
            [ testCase "Say nothing"
              <| fun _ ->
                  let Result = Main.SimpleFunction 1 2
                  Expect.equal Result 3 "Not an absolute unit"
              testCase "Say hello all"
              <| fun _ ->
                  let Result = Main.SimpleFunction 0 0
                  Expect.equal Result 0 "You didn't say hello" ]
